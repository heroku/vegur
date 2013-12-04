%% @copyright Heroku (2013)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Cowboy Client HTTP handler for hstub.
%% @end
-module(hstub_cc_handler).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-include("include/hstub_log.hrl").

-record(state,
        {backend_addr,
         backend_client}).

-define(BUFFER_LIMIT, 1024). % in bytes

init({_Any, http}, Req, []) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {Req2, State2} = route(Req, State),
    case connect(Req2, State2) of
        {connected, Req3, State3} ->
            proxy(Req3, State3);
        {Err, Req3, State3} ->
            respond_err(Err, Req3, State3)
    end.

respond_err(Error, Req, State) ->
    respond(503,
            [{<<"Content-Type">>, <<"text/plain">>}],
            io_lib:format("~p", [Error]),
            Req, State).

respond(Code, Headers, Body, Req, State) ->
    {ok, Req2} = cowboy_req:reply(Code,
                                  Headers,
                                  Body,
                                  Req),
    {ok, Req2, backend_close(State)}.


terminate(_Reason, _Req, _State) ->
    ok.

route(Req, State) ->
    case cowboy_req:host(Req) of
        {_Host, Req2} ->
            {Req2,
             State#state{backend_addr=hstub_app:config(backend)}}
    end.

connect(Req, State = #state{backend_addr={IP, Port}}) ->
    {ok, Client} = hstub_client:init([]),
    case hstub_client:connect(ranch_tcp,
                              ip_to_tuple(IP),
                              Port,
                              100,
                              Client) of
        {ok, Client2} ->
            {connected, Req, State#state{backend_client=Client2}};
        {error, _} = Err ->
            {Err, Req, State}
    end.

proxy(Req, State) ->
    {BackendReq, Req1} = parse_request(Req),
    case cowboy_req:meta(upgrade_requested, Req1, false) of
        {false, Req2} ->
            http_request(BackendReq, Req2, State);
        {true, Req2} ->
            http_upgrade(BackendReq, Req2, State)
    end.

send_request({Method, Headers, {stream,BodyLen}, URL, Path}, Req, State) ->
    %% Sends a request with a body yet to come through streaming. The BodyLen
    %% value can be either 'chunked' or an actual length.
    %% hstub_client:request_to_iolist will return a partial request with the
    %% correct headers in place, and the body can be sent later with sequential
    %% raw_request calls.
    Request = hstub_client:request_to_iolist(Method,
                                             request_headers(Headers),
                                             {stream,BodyLen},
                                             'HTTP/1.1',
                                             URL,
                                             Path),
    {Fun, FunState} = case BodyLen of
        chunked -> {fun decode_chunked/2, {undefined, 0}};
        _ -> {fun decode_raw/2, {0, BodyLen}}
    end,
    {ok, Req2} = cowboy_req:init_stream(Fun, FunState, fun decode_identity/1, Req),
    stream_request(Request, Req2, State);
send_request({Method, Headers, Body, URL, Path}, Req,
             State = #state{backend_client=Client}) ->
    %% We have a static, already known body, and can send it at once.
    Request = hstub_client:request_to_iolist(Method,
                                             request_headers(Headers),
                                             Body,
                                             'HTTP/1.1',
                                             URL,
                                             Path),
    case hstub_client:raw_request(Request, Client) of
        {ok, Client2} -> client_response(Req, State#state{backend_client=Client2});
        {error, _Err} = Err -> Err
    end.

%% Deal with the transfert of a large or chunked request body by going
%% from a cowboy stream to a raw hstub_client request.
stream_request(Buf, Req, State=#state{backend_client=Client}) ->
    {ok, _} = hstub_client:raw_request(Buf, Client),
    case cowboy_req:stream_body(Req) of
        {done, Req2} -> client_response(Req2, State);
        {ok, Data, Req2} -> stream_request(Data, Req2, State);
        {error, Err} -> {error, Err}
    end.

%% Fetch the response from the call sent.
client_response(Req, State=#state{backend_client=Client}) ->
    case hstub_client:response(Client) of
        {error, _} = Err -> Err;
        {ok, Status, RespHeaders, Client2} ->
            {ok, Status, RespHeaders, Req,
                 State#state{backend_client=Client2}}
    end.

http_request(BackendReq, Req, State) ->
    case send_request(BackendReq, Req, State) of
        {ok, Status, RespHeaders, Req2, State2} ->
            relay(Status, RespHeaders, Req2, State2);
        {error, _} = Err ->
            respond_err(Err, Req, backend_close(State))
    end.

http_upgrade(BackendReq, Req, State) ->
    case upgrade(BackendReq, Req, State) of
        {http, Status, RespHeaders, Req2, State2} ->
            relay(Status, RespHeaders, Req2, State2);
        {upgraded, Cowboy, HStub, Req2, State2} ->
            pipe(Cowboy, HStub, Req2, State2);
        {error, _} = Err ->
            respond_err(Err, Req, backend_close(State))
    end.

upgrade(Request, Req, State0) ->
    case send_request(Request, Req, State0) of
        {ok, 101, RespHeaders, Req2, State=#state{backend_client=Client}} ->
            %% fetch raw sockets and buffers
            {HStub={TransStub,SockStub}, BufStub, NewClient} = hstub_client:raw_socket(Client),
            {Cow={TransCow,SockCow}, BufCow, Req3} = cowboy_req:raw_socket(Req2),
            %% Send the response to the caller
            Headers = hstub_client:headers_to_iolist(request_headers(RespHeaders)),
            TransCow:send(SockCow,
                          [<<"HTTP/1.1 101 Switching Protocols\r\n">>,
                           Headers, <<"\r\n">>,
                           BufStub]),
            %% Flush leftover buffer data from the client, if any
            TransStub:send(SockStub, BufCow),
            {upgraded,
                Cow,    % acts as client
                HStub,  % acts as server
                Req3,
                State#state{backend_client=NewClient}};
        {ok, Code, RespHeaders, Req2, State} ->
            {http, Code, RespHeaders, Req2, State};
        {error, _Err} = Err ->
            Err
    end.

backend_close(State = #state{backend_client = undefined}) -> State;
backend_close(State = #state{backend_client = Client}) ->
    hstub_client:close(Client),
    State#state{backend_client = undefined}.

%% Dispatch data from hstub_client down into the cowboy connection, either
%% in batch or directly.
relay(Status, RespHeaders, Req, State = #state{backend_client = Client}) ->
    Headers = response_headers(RespHeaders),
    case hstub_client:body_type(Client) of
        {content_size, N} when N =< ?BUFFER_LIMIT ->
            relay_full_body(Status, Headers, Req, State);
        {content_size, N} ->
            relay_stream_body(Status, Headers, N, fun stream_body/2, Req, State);
        stream_close -> % unknown content-lenght, stream until connection close
            relay_stream_body(Status, Headers, undefined, fun stream_close/2, Req, State);
        chunked ->
            relay_chunked(Status, Headers, Req, State)
    end.

pipe(Cli, Serv, Req, State) ->
    ok = hstub_bytepipe:become(Cli, Serv, [{timeout, timer:seconds(55)}]),
    {ok, Req, State#state{backend_client=undefined}}.

%% The entire body is known and we can pipe it through as is.
relay_full_body(Status, Headers, Req, State = #state{backend_client=Client}) ->
    case hstub_client:response_body(Client) of
        {ok, Body, Client2} ->
            respond(Status, Headers, Body, Req, State#state{backend_client=Client2});
        {error, Reason} ->
            respond_err({error, Reason}, Req, backend_close(State))
    end.

%% The body is large and may need to be broken in multiple parts. Send them as
%% they come.
relay_stream_body(Status, Headers, Size, StreamFun, Req,
                  State=#state{backend_client=Client}) ->
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
            {ok, Req3, backend_close(State)}
    catch
        {stream_error, _} ->
            {shutdown, Req2, backend_close(State)}
    end.

relay_chunked(Status, Headers, Req, State = #state{backend_client = Client}) ->
    %% This is a special case. We stream pre-delimited chunks raw instead
    %% of using cowboy, which would have to recalculate and re-delimitate
    %% sizes all over after we parsed them first. We save time by just using
    %% raw chunks.
    {ok, Req2} = cowboy_req:chunked_reply(Status, Headers, Req),
    {RawSocket, Req3} = cowboy_req:raw_socket(Req2, [no_buffer]),
    case stream_chunked(RawSocket, Client) of
        {ok, Client2} ->
            {ok, Req3, State#state{backend_client=Client2}};
        {error, _} -> % uh-oh, we died during the transfer
            {shutdown, Req3, backend_close(State)}
    end.

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

%% Cowboy also allows to decode data further after one pass, say if it
%% was gzipped or something. For our use cases, we do not care about this
%% as we relay the information as-is, so this function does nothing.
decode_identity(Data) ->
    {ok, Data}.

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
            {ok, Client2};
        {error, Reason} ->
            {error, Reason}
    end.

parse_request(Req) ->
    {Method, Req2} =  cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {Host, Req4} = cowboy_req:host(Req3),
    {Port, Req5} = cowboy_req:port(Req4),
    {Headers, Req6} = cowboy_req:headers(Req5),
    %% We handle the request differently based on whether it's chunked,
    %% has a known length, or if it has no body at all.
    {Body, Req7} = case cowboy_req:has_body(Req6) of
        true ->
            case cowboy_req:body_length(Req6) of
                {undefined, ReqN} -> {{stream,chunked}, ReqN};
                {Length, ReqN} -> {{stream,Length}, ReqN}
            end;
        false ->
            {<<>>, Req6}
    end,
    {{Method,
      Headers,
      Body,
      iolist_to_binary([Host, ":", integer_to_list(Port)]),
      Path},
     Req7}.

ip_to_tuple(IP) when is_binary(IP) ->
    IPs = binary_to_list(IP),
    case inet:parse_address(IPs) of
        {ok, Parsed} ->
            Parsed;
        {error, einval} ->
            IPs
    end.

%% Strip Connection header on response.
response_headers(Headers0) ->
    lists:foldl(fun (F, H) ->
                        F(H)
                end,
                Headers0,
                [fun delete_keepalive_header/1
                ]).
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
