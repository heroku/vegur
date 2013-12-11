-module(hstub_proxy).

-define(BUFFER_LIMIT, 1024). % in bytes

-export([backend_connection/1
         ,send_request/7
         ,read_response/1
         ,upgrade/3
         ,relay/4]).

-spec backend_connection(Service) ->
                                {connected, Client} |
                                {error, any()} when
      Service :: hstub_service:service(),
      Client :: hstub_client:client().
backend_connection(Service) ->
    {IpAddress, Port} = hstub_service:backend(Service),
    {ok, Client} = hstub_client:init([]),
    case hstub_client:connect(ranch_tcp, IpAddress, Port,
                              100, Client) of
        {ok, Client1} ->
            {connected, Client1};
        {error, Reason} ->
            {error, Reason}
    end.

-spec send_request(Method, Headers, Body, Path, Url, Req, Client) ->
                            {done, Req, Client} |
                            {error, any()} when
      Body ::{stream, chunked|non_neg_integer()}|binary(),
      Method :: binary(),
      Headers :: [{binary(), binary()}]|[],
      Path :: binary(),
      Url :: binary(),
      Req :: cowboy_req:req(),
      Client :: hstub_client:client().
send_request(Method, Headers, {stream, BodyLen}, Path, Url, Req, Client) ->
    %% Sends a request with a body yet to come through streaming. The BodyLen
    %% value can be either 'chunked' or an actual length.
    %% hstub_client:request_to_iolist will return a partial request with the
    %% correct headers in place, and the body can be sent later with sequential
    %% raw_request calls.
    Request = hstub_client:request_to_iolist(Method,
                                             request_headers(Headers),
                                             {stream,BodyLen},
                                             'HTTP/1.1',
                                             Url,
                                             Path),
    {Fun, FunState} = case BodyLen of
        chunked -> {fun decode_chunked/2, {undefined, 0}};
        _ -> {fun decode_raw/2, {0, BodyLen}}
    end,
    {ok, Req2} = cowboy_req:init_stream(Fun, FunState, fun decode_identity/1, Req),
    stream_request(Request, Req2, Client);
send_request(Method, Headers, Body, Path, Url, Req, Client) ->
    %% We have a static, already known body, and can send it at once.
    Request = hstub_client:request_to_iolist(Method,
                                             request_headers(Headers),
                                             Body,
                                             'HTTP/1.1',
                                             Url,
                                             Path),
    case hstub_client:raw_request(Request, Client) of
        {ok, Client2} -> {done, Req, Client2};
        {error, _Err} = Err -> Err
    end.

-spec read_response(Client) ->
                           {ok, Code, Headers, Client} |
                           {error, Error} when
      Client :: hstub_client:client(),
      Code :: pos_integer(),
      Headers :: [{binary(), binary()}]|[],
      Error :: any().
read_response(Client) ->
    case hstub_client:response(Client) of
        {error, _} = Err -> Err;
        {ok, Code, RespHeaders, Client2} ->
            {ok, Code, RespHeaders, Client2}
    end.

-spec upgrade(Headers, Req, Client) ->
                     {done, Req} when
      Req :: cowboy_req:req(),
      Headers :: [{binary(), binary()}]|[],
      Client :: hstub_client:client().
upgrade(Headers, Req, BackendClient) ->
    %% fetch raw sockets and buffers
    {Server={TransStub,SockStub}, BufStub, _NewClient} = hstub_client:raw_socket(BackendClient),
    {Client={TransCow,SockCow}, BufCow, Req3} = cowboy_req:raw_socket(Req),
    %% Send the response to the caller
    Headers1 = hstub_client:headers_to_iolist(request_headers(Headers)),
    TransCow:send(SockCow,
                  [<<"HTTP/1.1 101 Switching Protocols\r\n">>,
                   Headers1, <<"\r\n">>,
                   BufStub]),
    %% Flush leftover buffer data from the client, if any
    TransStub:send(SockStub, BufCow),
    ok = hstub_bytepipe:become(Client, Server, [{timeout, timer:seconds(55)}]),
    backend_close(Client),
    {done, Req3}.

-spec relay(Status, Headers, Req, Client) ->
                   {ok, Req, Client} |
                   {error, Error, Req} when
      Status :: pos_integer(),
      Headers :: [{binary(), binary()}]|[],
      Req :: cowboy_req:req(),
      Client :: hstub_client:client(),
      Error :: any().
relay(Status, Headers, Req, Client) ->
    %% Dispatch data from hstub_client down into the cowboy connection, either
    %% in batch or directly.
    Headers = response_headers(Headers),
    case hstub_client:body_type(Client) of
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
    case hstub_client:response_body(Client) of
        {ok, Body, Client2} ->
            Req1 = respond(Status, Headers, Body, Req),
            {ok, Req1, backend_close(Client2)};
        {error, Error} ->
            backend_close(Client),
            {error, Error, Req}
    end.

%% The body is large and may need to be broken in multiple parts. Send them as
%% they come.
relay_stream_body(Status, Headers, Size, StreamFun, Req, Client) ->
    %% Use cowboy's partial response delivery to stream contents.
    %% We use exceptions (throws) to detect bad transfers and close
    %% both connections when this happens.
    Fun = fun(Socket, Transport) ->
        case StreamFun({Transport,Socket}, Client) of
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
            {error, Error, Req2}
    end.

relay_chunked(Status, Headers, Req, Client) ->
    %% This is a special case. We stream pre-delimited chunks raw instead
    %% of using cowboy, which would have to recalculate and re-delimitate
    %% sizes all over after we parsed them first. We save time by just using
    %% raw chunks.
    {ok, Req2} = cowboy_req:chunked_reply(Status, Headers, Req),
    {RawSocket, Req3} = cowboy_req:raw_socket(Req2, [no_buffer]),
    case stream_chunked(RawSocket, Client) of
        {ok, Client2} ->
            {ok, Req3, backend_close(Client2)};
        {error, Error} -> % uh-oh, we died during the transfer
            backend_close(Client),
            {error, Error, Req3}
    end.

stream_chunked({Transport,Sock}=Raw, Client) ->
    %% Fetch chunks one by one (including length and line-delimitation)
    %% and forward them over the raw socket.
    case hstub_client:stream_chunk(Client) of
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
            backend:close(Client),
            {error, Reason}
    end.

%% Deal with the transfert of a large or chunked request body by going
%% from a cowboy stream to a raw hstub_client request.
stream_request(Buffer, Req, Client) ->
    {ok, _} = hstub_client:raw_request(Buffer, Client),
    case cowboy_req:stream_body(Req) of
        {done, Req2} -> {done, Req2, Client};
        {ok, Data, Req2} -> stream_request(Data, Req2, Client);
        {error, Err} -> {error, Err}
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
    case hstub_chunked:stream_chunk(Data, Cont) of
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
    case hstub_client:stream_body(Client) of
        {ok, Data, Client2} ->
            Transport:send(Sock, Data),
            stream_body(Raw, Client2);
        {done, Client2} ->
            {ok, Client2};
        {error, Reason} ->
            {error, Reason}
    end.

stream_close({Transport,Sock}=Raw, Client) ->
    %% Stream the body until the connection is closed.
    case hstub_client:stream_close(Client) of
        {ok, Data, Client2} ->
            Transport:send(Sock, Data),
            stream_close(Raw, Client2);
        {done, Client2} ->
            {ok, Client2};
        {error, Reason} ->
            {error, Reason}
    end.


backend_close(undefined) -> undefined;
backend_close(Client) ->
    hstub_client:close(Client),
    undefined.

%% Strip Connection header on request.
request_headers(Headers0) ->
    lists:foldl(fun (F, H) ->
                        F(H)
                end,
                Headers0,
                [fun delete_keepalive_header/1
                ,fun delete_host_header/1
                ,fun add_connection_close/1
                ,fun delete_content_length_header/1
                ]).

%% Strip Connection header on response.
response_headers(Headers) ->
    lists:foldl(fun (F, H) ->
                        F(H)
                end,
                Headers,
                [fun delete_keepalive_header/1
                ]).

delete_keepalive_header(Hdrs) ->
    lists:delete({<<"connection">>, <<"keepalive">>}, Hdrs).

delete_host_header(Hdrs) ->
    lists:keydelete(<<"host">>, 1, Hdrs).

delete_content_length_header(Hdrs) ->
    lists:keydelete(<<"content-length">>, 1, Hdrs).

add_connection_close(Hdrs) ->
    case lists:keymember(<<"connection">>, 1, Hdrs) of
        true -> Hdrs;
        false -> [{<<"connection">>, <<"close">>} | Hdrs]
    end.
