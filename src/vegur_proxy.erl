-module(vegur_proxy).

-define(BUFFER_LIMIT, 1024). % in bytes

-export([backend_connection/1
         ,send_headers/7
         ,send_body/7
         ,read_backend_response/2
         ,upgrade/3
         ,relay/4]).

-type error_blame() :: upstream|downstream.

-spec backend_connection(ServiceBackend) ->
                                {connected, Client} |
                                {error, any()} when
      ServiceBackend :: vegur_interface:service_backend(),
      Client :: vegur_client:client().
backend_connection({IpAddress, Port}) ->
    {ok, Client} = vegur_client:init([]),
    case vegur_client:connect(ranch_tcp, IpAddress, Port,
                              100, Client) of
        {ok, Client1} ->
            {connected, Client1};
        {error, Reason} ->
            {error, Reason}
    end.

-spec send_headers(Method, Headers, Body, Path, Url, Req, Client) ->
                          {done, Blame, Req, Client} |
                          {error, any()} when
      Body :: {stream, chunked|non_neg_integer()}|binary(),
      Method :: binary(),
      Headers :: [{binary(), binary()}]|[],
      Path :: binary(),
      Url :: binary(),
      Req :: cowboy_req:req(),
      Client :: vegur_client:client(),
      Blame :: error_blame().
send_headers(Method, Headers, Body, Path, Url, Req, Client) ->
    %% Sends a request with a body yet to come through streaming. The BodyLen
    %% value can be either 'chunked' or an actual length.
    %% vegur_client:request_to_iolist will return a partial request with the
    %% correct headers in place, and the body can be sent later with sequential
    %% raw_request calls.
    {Type, _} = cowboy_req:meta(request_type, Req, []),
    IoHeaders = vegur_client:request_to_headers_iolist(Method,
                                                       request_headers(Headers, Type),
                                                       Body,
                                                       'HTTP/1.1',
                                                       Url,
                                                       Path),
    {ok, _} = vegur_client:raw_request(IoHeaders, Client),
    {Cont, Req1} = cowboy_req:meta(continue, Req, []),
    case Cont of
        continue ->
            negotiate_continue(Body, Req1, Client);
        _ ->
            {done, Req1, Client}
    end.

send_body(_Method, _Header, Body, _Path, _Url, Req, BackendClient) ->
    case Body of
        {stream, BodyLen} ->
            {Fun, FunState} = case BodyLen of
                chunked -> {fun decode_chunked/2, {undefined, 0}};
                BodyLen -> {fun decode_raw/2, {0, BodyLen}}
            end,
            {ok, Req2} = cowboy_req:init_stream(Fun, FunState, fun decode_identity/1, Req),
            %% use headers & body to stream correctly
            stream_request(Req2, BackendClient);
        Body ->
            {ok, _} = vegur_client:raw_request(Body, BackendClient),
            {done, Req, BackendClient}
    end.

negotiate_continue(Body, Req, BackendClient) ->
    Timeout = timer:seconds(vegur_app:config(idle_timeout, 55)),
    negotiate_continue(Body, Req, BackendClient, Timeout).

negotiate_continue(_, _, _, Timeout) when Timeout =< 0 ->
    {error, upstream, Timeout};
negotiate_continue(Body, Req, BackendClient, Timeout) ->
    %% In here, we must await the 100 continue from the BackendClient
    %% *OR* wait until cowboy (front-end) starts sending data.
    %% Because there is a timeout before which a client may send data,
    %% and that we may have looked for a suitable backend for a considerable
    %% amount of time, always start by looking over the client connection.
    %% If the client sends first, we then *may* have to intercept the first
    %% 100 continue and not pass it on.
    %% Strip the 'continue' request type from meta!
    Wait = timer:seconds(1),
    case cowboy_req:buffer_data(0, 0, Req) of
        {ok, Req1} ->
            {done, Req1, BackendClient};
        {error, timeout} ->
            case vegur_client:buffer_data(0, Wait, BackendClient) of
                {ok, BackendClient1} ->
                    case read_response(BackendClient1) of
                        {ok, 100, _RespHeaders, _BackendClient2} ->
                            %% We don't carry the headers on a 100 Continue
                            %% for a simpler implementation -- there is no
                            %% header prescribed for it in the spec anyway.
                            Req1 = send_continue(Req, BackendClient),
                            %% We use the original client so that no state
                            %% change due to 100 Continue is observable.
                            {done, Req1, BackendClient};
                        {ok, Code, RespHeaders, BackendClient2} ->
                            {ok, Code, RespHeaders, BackendClient2};
                        {error, Blame, Reason} ->
                            {error, Blame, Reason}
                    end;
                {error, timeout} ->
                    negotiate_continue(Body, Req, BackendClient, Timeout-Wait);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, upstream, Error}
    end.

-spec read_response(Client) ->
                           {ok, Code, Headers, Client} |
                           {error, Blame, Error} when
      Client :: vegur_client:client(),
      Code :: pos_integer(),
      Headers :: [{binary(), binary()}]|[],
      Blame :: error_blame(),
      Error :: any().
read_response(Client) ->
    case vegur_client:response(Client) of
        {error, Error} ->
            {error, downstream, Error};
        {ok, Code, RespHeaders, Client2} ->
            {ok, Code, RespHeaders, Client2}
    end.

%% This function works like read_response, but actually handles
%% the 100-Continue business to keep it out of the regular request flow
%% for the middleware.
-spec read_backend_response(Req, Client) ->
                           {ok, Code, Headers, Req, Client} |
                           {error, Blame, Error} when
      Req :: cowboy_req:req(),
      Client :: vegur_client:client(),
      Code :: pos_integer(),
      Headers :: [{binary(), binary()}]|[],
      Blame :: error_blame(),
      Error :: any().
read_backend_response(Req, Client) ->
    case read_response(Client) of
        {error, Blame, Error} -> {error, Blame, Error};
        {ok, Code, RespHeaders, Client1} ->
            {Cont, Req1} = cowboy_req:meta(continue, Req, []),
            case {Code, Cont} of
                {100, continue} ->
                    %% Leftover from Continue due to race condition between
                    %% client and server. Forward to client, which should
                    %% deal with it.
                    Req2 = send_continue(Req1, Client),
                    read_backend_response(Req2, Client1);
                {100, continued} ->
                    {error, downstream, non_terminal_status_after_continue};
                {100, _} ->
                    case cowboy_req:version(Req1) of
                        {'HTTP/1.0', Req2} ->
                            %% Http1.0 client without expect: 100-continue
                            %% Strip as per RFC.
                            read_backend_response(Req2, Client1);
                        {_, Req2} ->
                            %% Forward it. Older HTTP 1.1 servers may send
                            %% these or no reason, and clients should handle
                            %% them.
                            Req3 = send_continue(Req2, Client),
                            read_backend_response(Req3, Client1)
                    end;
                _ ->
                    {ok, Code, RespHeaders, Req1, Client1}
            end
    end.

send_continue(Req, BackendClient) ->
    HTTPVer = atom_to_binary(vegur_client:version(BackendClient), latin1),
    {{Transport,Socket}, _} = cowboy_req:raw_socket(Req),
    Transport:send(Socket,
        [HTTPVer, <<" 100 Continue\r\n\r\n">>]),
    %% Got it. Now clean up the '100 Continue' state from
    %% the request, and mark it as handled
    cowboy_req:set_meta(continue, continued, Req).

-spec upgrade(Headers, Req, Client) ->
                     {done, Req, Client} when
      Req :: cowboy_req:req(),
      Headers :: [{binary(), binary()}]|[],
      Client :: vegur_client:client().
upgrade(Headers, Req, BackendClient) ->
    %% fetch raw sockets and buffers
    {Server={TransStub,SockStub}, BufStub, _NewClient} = vegur_client:raw_socket(BackendClient),
    {Client={TransCow,SockCow}, BufCow, Req3} = cowboy_req:raw_sockbuf(Req),
    %% Send the response to the caller
    Headers1 = vegur_client:headers_to_iolist(upgrade_response_headers(Headers)),
    TransCow:send(SockCow,
                  [<<"HTTP/1.1 101 Switching Protocols\r\n">>,
                   Headers1, <<"\r\n">>,
                   BufStub]),
    %% Flush leftover buffer data from the client, if any
    TransStub:send(SockStub, BufCow),
    CloseFun = fun(TransC, PortC, TransS, PortS, Event) ->
        BackendClient1 = vegur_client:set_stats(BackendClient),
        ok = vegur_bytepipe:cb_close(TransC, PortC, TransS, PortS, Event),
        BackendClient1
    end,
    Timeout = timer:seconds(vegur_app:config(idle_timeout, 55)),
    BackendClient1 = vegur_bytepipe:become(Client, Server, [{timeout, Timeout},
                                                            {on_close, CloseFun},
                                                            {on_timeout, CloseFun}]),
    {done, Req3, backend_close(BackendClient1)}.

-spec relay(Status, Headers, Req, Client) ->
                   {ok, Req, Client} |
                   {error, Blame, Error, Req} when
      Status :: pos_integer(),
      Headers :: [{binary(), binary()}]|[],
      Req :: cowboy_req:req(),
      Client :: vegur_client:client(),
      Error :: any(),
      Blame :: error_blame().
relay(Status, HeadersRaw, Req, Client) ->
    %% Dispatch data from vegur_client down into the cowboy connection, either
    %% in batch or directly.
    Headers = case connection_type(Status, Req, Client) of
        keepalive -> add_connection_keepalive_header(response_headers(HeadersRaw));
        close  -> add_connection_close_header(response_headers(HeadersRaw))
    end,
    case vegur_client:body_type(Client) of
        {content_size, N} when N =< ?BUFFER_LIMIT ->
            relay_full_body(Status, Headers, Req, Client);
        {content_size, N} ->
            relay_stream_body(Status, Headers, N, fun stream_body/2, Req, Client);
        stream_close -> % unknown content-lenght, stream until connection close
            relay_stream_body(Status, Headers, undefined, fun stream_close/2, Req, Client);
        chunked ->
            relay_chunked(Status, Headers, Req, Client);
        no_body ->
            relay_no_body(Status, Headers, Req, Client)
    end.

%% There is no body to relay
relay_no_body(Status, Headers, Req, Client) ->
    Req1 = respond(Status, Headers, <<>>, Req),
    {ok, Req1, backend_close(Client)}.

%% The entire body is known and we can pipe it through as is.
relay_full_body(Status, Headers, Req, Client) ->
    case wait_for_body(Status, Req) of
        dont_wait ->
            {content_size, N} = vegur_client:body_type(Client),
            relay_stream_body(Status, Headers, N, fun stream_nothing/2, Req, Client);
        wait ->
            case vegur_client:response_body(Client) of
                {ok, Body, Client2} ->
                    Req1 = respond(Status, Headers, Body, Req),
                    {ok, Req1, backend_close(Client2)};
                {error, Error} ->
                    backend_close(Client),
                    {error, downstream, Error, Req}
            end
    end.

%% The body is large and may need to be broken in multiple parts. Send them as
%% they come.
relay_stream_body(Status, Headers, Size, StreamFun, Req, Client) ->
    %% Use cowboy's partial response delivery to stream contents.
    %% We use exceptions (throws) to detect bad transfers and close
    %% both connections when this happens.
    FinalFun = case wait_for_body(Status, Req) of
        wait -> StreamFun;
        dont_wait -> fun stream_nothing/2
    end,
    Fun = fun(Socket, Transport) ->
        case FinalFun({Transport,Socket}, Client) of
            {ok, _Client2} -> ok;
            {error, Reason} -> throw({stream_error, Reason})
        end
    end,
    Req2 = case Size of
        undefined -> cowboy_req:set_resp_body_fun(Fun, Req); % end on close
        _ -> cowboy_req:set_resp_body_fun(Size, Fun, Req)    % end on size
    end,
    try cowboy_req:reply(Status, Headers, Req2) of
        {ok, Req3} ->
            {ok, Req3, backend_close(Client)}
    catch
        {stream_error, Error} ->
            backend_close(Client),
            {error, downstream, Error, Req2}
    end.

relay_chunked(Status, Headers, Req, Client) ->
    {Version, Req2} = cowboy_req:version(Req),
    relay_chunked(Version, Status, Headers, Req2, Client).

relay_chunked('HTTP/1.1', Status, Headers, Req, Client) ->
    %% This is a special case. We stream pre-delimited chunks raw instead
    %% of using cowboy, which would have to recalculate and re-delimitate
    %% sizes all over after we parsed them first. We save time by just using
    %% raw chunks.
    {ok, Req2} = cowboy_req:chunked_reply(Status, Headers, Req),
    {RawSocket, Req3} = cowboy_req:raw_socket(Req2),
    case wait_for_body(Status, Req) of
        dont_wait ->
            {ok, Req3, backend_close(Client)};
        wait ->
            case stream_chunked(RawSocket, Client) of
                {ok, Client2} ->
                    {ok, Req3, backend_close(Client2)};
                {error, Blame, Error} -> % uh-oh, we died during the transfer
                    backend_close(Client),
                    {error, Blame, Error, Req3}
            end
    end;
relay_chunked('HTTP/1.0', Status, Headers, Req, Client) ->
    %% This case means that we're forwarding chunked encoding to an
    %% older client that doesn't support it. The way around this is to
    %% stream the data as-is, but use no `content-length' header *AND* a
    %% `connection: close' header to implicitly delimit the request
    %% as streaming unknown-size HTTP.
    relay_stream_body(Status, delete_transfer_encoding_header(Headers),
                      undefined, fun stream_unchunked/2, Req, Client).

stream_chunked({Transport,Sock}=Raw, Client) ->
    %% Fetch chunks one by one (including length and line-delimitation)
    %% and forward them over the raw socket.
    case vegur_client:stream_chunk(Client) of
        {ok, Data, Client2} ->
            Transport:send(Sock, Data),
            stream_chunked(Raw, Client2);
        {more, _Len, Data, Client2} ->
            Transport:send(Sock, Data),
            stream_chunked(Raw, Client2);
        {done, Data, Client2} ->
            Transport:send(Sock, Data),
            {ok, backend_close(Client2)};
        {error, Reason} ->
            backend_close(Client),
            {error, downstream, Reason}
    end.

stream_unchunked({Transport,Sock}=Raw, Client) ->
    %% Fetch chunks one by one (excluding length and line-delimitation)
    %% and forward them over the raw socket.
    case vegur_client:stream_unchunk(Client) of
        {ok, Data, Client2} ->
            Transport:send(Sock, Data),
            stream_unchunked(Raw, Client2);
        {more, _Len, Data, Client2} ->
            Transport:send(Sock, Data),
            stream_unchunked(Raw, Client2);
        {done, Data, Client2} ->
            Transport:send(Sock, Data),
            {ok, backend_close(Client2)};
        {error, Reason} ->
            backend_close(Client),
            {error, Reason}
    end.

%% Deal with the transfer of a large or chunked request body by
%% going from a cowboy stream to raw htsub_client requests
stream_request(Req, Client) ->
    case cowboy_req:stream_body(Req) of
        {done, Req2} -> {done, Req2, Client};
        {ok, Data, Req2} -> stream_request(Data, Req2, Client);
        {error, Err} -> {error, upstream, Err}
    end.

stream_request(Buffer, Req, Client) ->
    {ok, _} = vegur_client:raw_request(Buffer, Client),
    case cowboy_req:stream_body(Req) of
        {done, Req2} -> {done, Req2, Client};
        {ok, Data, Req2} -> stream_request(Data, Req2, Client);
        {error, Err} -> {error, upstream, Err}
    end.

%% Cowboy also allows to decode data further after one pass, say if it
%% was gzipped or something. For our use cases, we do not care about this
%% as we relay the information as-is, so this function does nothing.
decode_identity(Data) ->
    {ok, Data}.

%% Custom decoder for Cowboy that will allow to stream data without modifying
%% it, in bursts, directly to the dyno without accumulating it in memory.
decode_raw(Data, {Streamed, Total}) when Streamed + byte_size(Data) < Total ->
    %% Still a lot to go, we return it all as a frame
    {ok, Data, <<>>, {Streamed+iolist_size(Data), Total}};
decode_raw(Data, {Streamed, Total}) ->
    %% Last batch, but we may have more than we asked for.
    Size = Total-Streamed,
    <<Data2:Size/binary, Rest/binary>> = Data,
    {done, Data2, Total, Rest}.

%% Custom decoder for Cowboy that will allow to return chunks in streams while
%% still giving us a general idea when a chunk begins and ends, and when the
%% entire request is cleared. Can deal with partial chunks for cases where
%% the user sends in multi-gigabyte chunks to mess with us.
decode_chunked(Data, {Cont, Total}) ->
    case vegur_chunked:stream_chunk(Data, Cont) of
        {done, Buf, Rest} ->
            %% Entire request is over
            {done, Buf, Total+iolist_size(Buf), Rest};
        {chunk, Buf, Rest} ->
            %% Chunk is done, but more to come
            {ok, Buf, Rest, {undefined, Total+iolist_size(Buf)}};
        {more, _Len, Buf, Cont2} ->
            %% Not yet done on the current chunk, but keep going.
            {ok, Buf, <<>>, {Cont2, Total}}
    end.

respond(Status, Headers, Body, Req) ->
    {ok, Req1} = cowboy_req:reply(Status, Headers, Body, Req),
    Req1.

stream_body({Transport,Sock}=Raw, Client) ->
    %% Stream the body until as much data is sent as there
    %% was in its content-length initially.
    case vegur_client:stream_body(Client) of
        {ok, Data, Client2} ->
            Transport:send(Sock, Data),
            stream_body(Raw, Client2);
        {done, Client2} ->
            {ok, vegur_client:set_stats(Client2)};
        {error, Reason} ->
            {error, Reason}
    end.

stream_close({Transport,Sock}=Raw, Client) ->
    %% Stream the body until the connection is closed.
    case vegur_client:stream_close(Client) of
        {ok, Data, Client2} ->
            Transport:send(Sock, Data),
            stream_close(Raw, Client2);
        {done, Client2} ->
            {ok, vegur_client:set_stats(Client2)};
        {error, Reason} ->
            {error, downstream, Reason}
    end.

stream_nothing(_Raw, Client) ->
    {ok, Client}.

%% We should close the connection whenever we get an Expect: 100-Continue
%% that got answered with a final status code.
connection_type(Status, Req, Client) ->
    {Cont, _} = cowboy_req:meta(continue, Req, []),
    %% If we haven't received a 100 continue to forward after having
    %% received an expect AND this is a final status, then we should
    %% close the connection.
    case Cont =:= continue andalso Status >= 200 of
        true ->
            close;
        false ->
            %% Honor the client's decision, except if the response has no
            %% content-length, in which case closing is mandatory
            case vegur_client:body_type(Client) of
                stream_close ->
                    close;
                chunked ->
                    %% Chunked with an HTTP/1.0 client gets turned to a close
                    %% to allow proper data streaming.
                    case cowboy_req:version(Req) of
                        {'HTTP/1.0', _} -> close;
                        {'HTTP/1.1', _} -> cowboy_req:get(connection, Req)
                    end;
                _ ->
                    cowboy_req:get(connection, Req)
            end
    end.

wait_for_body(204, _Req) -> dont_wait;
wait_for_body(304, _Req) -> dont_wait;
wait_for_body(_, Req) ->
    case cowboy_req:method(Req) of
        {<<"HEAD">>, _} -> dont_wait;
        _ -> wait
    end.



backend_close(Client) ->
    vegur_client:close(Client).

%% Strip Connection header on request.
request_headers(Headers0, Type) ->
    HeaderFuns = case Type of
        [upgrade] ->
            [fun delete_host_header/1
            ,fun delete_hop_by_hop/1
            ,fun add_connection_upgrade_header/1
            ,fun delete_content_length_header/1];
        _ ->
            [fun delete_host_header/1
            ,fun delete_hop_by_hop/1
            ,fun add_connection_close_header/1
            ,fun delete_content_length_header/1]
    end,
    lists:foldl(fun (F, H) -> F(H) end, Headers0, HeaderFuns).

%% Strip Hop-by-hop headers on a response that is being
%% upgraded
upgrade_response_headers(Headers) ->
    lists:foldl(fun (F, H) ->
                        F(H)
                end,
                Headers,
                [fun delete_hop_by_hop/1,
                 fun add_connection_upgrade_header/1
                ]).

%% Strip Hop-by-hop headers on response
response_headers(Headers) ->
    lists:foldl(fun (F, H) ->
                        F(H)
                end,
                Headers,
                [fun delete_hop_by_hop/1
                ]).

delete_host_header(Hdrs) -> delete_all(<<"host">>, Hdrs).

delete_content_length_header(Hdrs) -> delete_all(<<"content-length">>, Hdrs).

delete_transfer_encoding_header(Hdrs) -> delete_all(<<"transfer-encoding">>, Hdrs).

%% Hop by Hop Headers we care about removing. We remove most of them but
%% "Proxy-Authentication" for historical reasons, "Upgrade" because we pass
%% it through, "Transfer-Encoding" because we restrict to 'chunked' and pass
%% it through.
delete_hop_by_hop([]) -> [];
delete_hop_by_hop([{<<"connection">>, _} | Hdrs]) -> delete_hop_by_hop(Hdrs);
delete_hop_by_hop([{<<"te">>, _} | Hdrs]) -> delete_hop_by_hop(Hdrs);
delete_hop_by_hop([{<<"keep-alive">>, _} | Hdrs]) -> delete_hop_by_hop(Hdrs);
delete_hop_by_hop([{<<"proxy-authorization">>, _} | Hdrs]) -> delete_hop_by_hop(Hdrs);
delete_hop_by_hop([{<<"trailer">>, _} | Hdrs]) -> delete_hop_by_hop(Hdrs);
delete_hop_by_hop([Hdr|Hdrs]) -> [Hdr | delete_hop_by_hop(Hdrs)].

%% We need to traverse the entire list because a user could have
%% injected more than one instance of the same header, and cowboy
%% doesn't coalesce headers for us.
delete_all(_, []) -> [];
delete_all(Key, [{Key,_} | Hdrs]) -> delete_all(Key, Hdrs);
delete_all(Key, [H|Hdrs]) -> [H | delete_all(Key, Hdrs)].

add_connection_close_header(Hdrs) ->
    case lists:keymember(<<"connection">>, 1, Hdrs) of
        true -> Hdrs;
        false -> [{<<"connection">>, <<"close">>} | Hdrs]
    end.

add_connection_keepalive_header(Hdrs) ->
    case lists:keymember(<<"connection">>, 1, Hdrs) of
        true -> Hdrs;
        false -> [{<<"connection">>, <<"keep-alive">>} | Hdrs]
    end.

add_connection_upgrade_header(Hdrs) ->
    case lists:keymember(<<"connection">>, 1, Hdrs) of
        true -> Hdrs;
        false -> [{<<"connection">>, <<"upgrade">>} | Hdrs]
    end.
