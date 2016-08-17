%%% Copyright (c) 2013-2015, Heroku Inc <routing-feedback@heroku.com>.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(vegur_proxy).

-define(UPSTREAM_BODY_BUFFER_LIMIT, 65536). % 64kb, in bytes
-define(DOWNSTREAM_BODY_BUFFER_LIMIT, 65536). % 64kb, in bytes

-export([backend_connection/1
         ,send_headers/7
         ,send_body/7
         ,read_backend_response/2
         ,upgrade/3
         ,relay/5
         ,reps_left/0]).

-type error_blame() :: 'undefined' % either/unknown
                     | 'upstream' % client
                     | 'downstream'. % back-end

%% @doc
%% Open up a connection to the service backend.
%% @end
%% This function requires cheating and using the process dictionary
%% a lot because we cannot have a stored connection-long piece of
%% data in cowboyku as is. Patching it would require modifying
%% cowboyku_protocol to extract and pass some data around many
%% iterations of cowboyku_req.
-spec backend_connection(ServiceBackend) ->
                                {connected, Client} |
                                {error, any()} when
      ServiceBackend :: vegur_interface:service_backend(),
      Client :: vegur_client:client().
backend_connection({keepalive, {Type, {IpAddress, Port}}}) ->
    %% Reuse the connection if told to and if one exists and has been
    %% stored. If a connection exists, the `default' value provided is
    %% ignored; if a connection does not exist, then the value will
    %% be used instead.
    put(should_keepalive, true),
    case {get(last_backend), get(reuse), Type} of
        {_, undefined, _} ->
            put(last_backend, {IpAddress, Port}),
            start_backend_connection({IpAddress, Port});
        {{_OldAddress, _OldPort}, Client, default} ->
            %% Set the delta on a new connection so that moving forwards,
            %% metrics are accurate
            {connected, vegur_client:reset_log(vegur_client:set_delta(Client))};
        {_, Client, _} -> % `new' Type falls through here also; acts like fresh
            erase(reuse),
            put(last_backend, {IpAddress, Port}),
            %% force close the connection
            put(should_keepalive, false),
            backend_close(Client),
            put(should_keepalive, true),
            %% start the new one
            start_backend_connection({IpAddress, Port})
    end;
backend_connection({IpAddress, Port}) ->
    %% Close any previously keepalive req we may have had
    case get(reuse) of
        undefined -> ok;
        Client ->
            erase(reuse),
            backend_close(Client)
    end,
    %% Set all values to mandate not reusing keepalive.
    put(should_keepalive, false),
    erase(last_backend),
    start_backend_connection({IpAddress, Port}).

start_backend_connection({IpAddress, Port}) ->
    TcpBufSize = vegur_utils:config(client_tcp_buffer_limit),
    {ok, Client} = vegur_client:init([{reuseaddr, true},
                                      {packet_size, TcpBufSize},
                                      {recbuf, TcpBufSize}]),
    case vegur_client:connect(ranch_tcp, IpAddress, Port, Client) of
        {ok, Client1} ->
            {connected, Client1};
        {error, Reason} ->
            {error, Reason}
    end.

-spec send_headers(Method, Headers, Body, Path, Url, Req, Client) ->
                          {done, Req, Client} |
                          {ok, Code, Status, Headers, Client} |
                          {error, Blame, any()} when
      Method :: binary(),
      Headers :: [{binary(), binary()}]|[],
      Body :: {stream, chunked|non_neg_integer()} | binary() | iodata(),
      Path :: binary(),
      Url :: binary(),
      Req :: cowboyku_req:req(),
      Client :: vegur_client:client(),
      Code :: non_neg_integer(),
      Status :: binary(),
      Blame :: error_blame().
send_headers(Method, Headers, Body, Path, Url, Req, Client) ->
    %% Sends a request with a body yet to come through streaming. The BodyLen
    %% value can be either 'chunked' or an actual length.
    %% vegur_client:request_to_iolist will return a partial request with the
    %% correct headers in place, and the body can be sent later with sequential
    %% raw_request calls.
    {Type, _} = cowboyku_req:meta(request_type, Req, []),
    ShouldKeepalive = get(should_keepalive) =:= true
                      andalso cowboyku_req:get(connection, Req) =:= keepalive,
    Headers1 = vegur_headers:request_headers(Headers, Type, ShouldKeepalive),
    IoHeaders = vegur_client:request_to_headers_iolist(Method,
                                                       Headers1,
                                                       Body,
                                                       'HTTP/1.1',
                                                       Url,
                                                       Path),
    case vegur_client:raw_request(IoHeaders, Client) of
        {ok, Client2} ->
            {Cont, Req1} = cowboyku_req:meta(continue, Req, []),
            case Cont of
                continue ->
                    negotiate_continue(Body, Req1, Client2);
                _ ->
                    {done, Req1, Client2}
            end;
        {error, Err} ->
            {error, downstream, Err}
    end.

send_body(_Method, Headers, Body, _Path, _Url, Req, BackendClient) ->
    case Body of
        {stream, BodyLen} -> % the body is yet to be fetched
            {Fun, FunState} = choose_body_stream_type(Headers, BodyLen),
            {ok, Req2} = cowboyku_req:init_stream(Fun, FunState, fun decode_identity/1, Req),
            stream_request(Req2, BackendClient);
        Body -> % the body is all there.
            case vegur_client:raw_request(Body, BackendClient) of
                {ok, BackendClient2} -> {done, Req, BackendClient2};
                {error, Err} -> {error, downstream, Err}
            end
    end.

%% depending on the type of body, set the right streaming function
choose_body_stream_type(_Headers, chunked) ->
    {fun decode_chunked/2, {undefined, undefined, 0}};
choose_body_stream_type(_Headers, BodyLen) ->
    {fun decode_raw/2, {0, BodyLen}}.


negotiate_continue(Body, Req, BackendClient) ->
    Timeout = timer:seconds(vegur_utils:config(idle_timeout)),
    negotiate_continue(Body, Req, BackendClient, Timeout).

negotiate_continue(_, _, _, Timeout) when Timeout =< 0 ->
    {error, upstream, timeout};
negotiate_continue(Body, Req, BackendClient, Timeout) ->
    %% In here, we must await the 100 continue from the BackendClient
    %% *OR* wait until cowboyku (front-end) starts sending data.
    %% Because there is a timeout before which a client may send data,
    %% and that we may have looked for a suitable backend for a considerable
    %% amount of time, always start by looking over the client connection.
    %% If the client sends first, we then *may* have to intercept the first
    %% 100 continue and not pass it on.
    %% Strip the 'continue' request type from meta!
    Wait = timer:seconds(1),
    case cowboyku_req:buffer_data(0, 0, Req) of
        {ok, Req1} ->
            {done, Req1, BackendClient};
        {error, timeout} ->
            case vegur_client:buffer_data(0, Wait, BackendClient) of
                {ok, BackendClient1} ->
                    case read_response(BackendClient1) of
                        {ok, 100, _Status, _RespHeaders, _BackendClient2} ->
                            %% We don't carry the headers on a 100 Continue
                            %% for a simpler implementation -- there is no
                            %% header prescribed for it in the spec anyway.
                            Req1 = send_continue(Req, BackendClient),
                            %% We use the original client so that no state
                            %% change due to 100 Continue is observable.
                            %% -- maybe this messes up some buffers though, in
                            %%    the case a server decided to be a jerk and
                            %%    send a 100-continue and the response immediately?
                            {done, Req1, BackendClient};
                        {ok, Code, Status, RespHeaders, BackendClient2} ->
                            {ok, Code, Status, RespHeaders, BackendClient2};
                        {error, Blame, Reason} ->
                            {error, Blame, Reason}
                    end;
                {error, timeout} ->
                    negotiate_continue(Body, Req, BackendClient, Timeout-Wait);
                {error, Error} ->
                    {error, downstream, Error}
            end;
        {error, Error} ->
            {error, upstream, Error}
    end.

-spec read_response(Client) ->
                           {ok, Code, Status, Headers, Client} |
                           {error, Blame, Error} when
      Client :: vegur_client:client(),
      Code :: pos_integer(),
      Status :: binary(),
      Headers :: [{binary(), binary()}]|[],
      Blame :: error_blame(),
      Error :: any().
read_response(Client) ->
    case vegur_client:response(Client) of
        {error, Error} ->
            {error, downstream, Error};
        {ok, Code, Status, RespHeaders, Client2} ->
            {ok, Code, Status, RespHeaders, Client2}
    end.

%% This function works like read_response, but actually handles
%% the 100-Continue business to keep it out of the regular request flow
%% for the middleware.
-spec read_backend_response(Req, Client) ->
                           {ok, Code, Status, Headers, Req, Client} |
                           {error, Blame, Error} when
      Req :: cowboyku_req:req(),
      Client :: vegur_client:client(),
      Code :: pos_integer(),
      Status :: binary(),
      Headers :: [{binary(), binary()}]|[],
      Blame :: error_blame(),
      Error :: any().
read_backend_response(Req, Client) ->
    case read_response(Client) of
        {error, Blame, Error} -> {error, Blame, Error};
        {ok, Code, Status, RespHeaders, Client1} ->
            {Cont, Req1} = cowboyku_req:meta(continue, Req, []),
            case {Code, Cont} of
                {100, continue} ->
                    %% Leftover from Continue due to race condition between
                    %% client and server when the client stops waiting for
                    %% the server and sends data, and the server responds
                    %% at exactly that moment. Forward to client, which should
                    %% deal with it.
                    Req2 = send_continue(Req1, Client),
                    read_backend_response(Req2, Client1);
                {100, continued} ->
                    {error, downstream, non_terminal_status_after_continue};
                {100, _} -> % 100 continue to req without the expect header
                    case cowboyku_req:version(Req1) of
                        {'HTTP/1.0', Req2} ->
                            %% HTTP/1.0 client without expect: 100-continue
                            %% Strip as per RFC.
                            read_backend_response(Req2, Client1);
                        {_, Req2} ->
                            %% Forward it. Older HTTP/1.1 servers may send
                            %% these or no reason, and clients should handle
                            %% them.
                            Req3 = send_continue(Req2, Client),
                            read_backend_response(Req3, Client1)
                    end;
                _ ->
                    {ok, Code, Status, RespHeaders, Req1, Client1}
            end
    end.

send_continue(Req, BackendClient) ->
    HTTPVer = atom_to_binary(vegur_client:version(BackendClient), latin1),
    {Transport,Socket} = vegur_utils:borrow_cowboyku_socket(Req),
    Transport:send(Socket,
        [HTTPVer, <<" 100 Continue\r\n\r\n">>]),
    %% Got it. Now clean up the '100 Continue' state from
    %% the request, and mark it as handled
    cowboyku_req:set_meta(continue, continued, Req).

-spec upgrade(Headers, Req, Client) ->
                     {done, Req, Client} |
                     {timeout, Req, Client} when
      Req :: cowboyku_req:req(),
      Headers :: [{binary(), binary()}]|[],
      Client :: vegur_client:client().
upgrade(Headers, Req, BackendClient) ->
    {Client, Server, Req2} = upgrade_init(Headers, Req, BackendClient),
    %% Set up handlers for the bytepipe
    CloseFun = fun(TransC, PortC, TransS, PortS, Event) ->
        BackendClient1 = vegur_client:set_stats(BackendClient),
        ok = vegur_bytepipe:cb_close(TransC, PortC, TransS, PortS, Event),
        BackendClient1
    end,
    TimeoutFun = fun(TransC, PortC, TransS, PortS, Event) ->
        BackendClient1 = CloseFun(TransC, PortC, TransS, PortS, Event),
        {timeout, BackendClient1}
    end,
    Timeout = timer:seconds(vegur_utils:config(idle_timeout)),
    Res = vegur_bytepipe:become(Client, Server, [{timeout, Timeout},
                                                 {on_close, CloseFun},
                                                 {on_timeout, TimeoutFun}]),
    case Res of
        {timeout, BackendClient1} ->
            {timeout, Req2, BackendClient1};
        BackendClient1 ->
            {done, Req2, backend_close(BackendClient1)}
    end.

%% Move all the websocket data to be into a clean initial state
%% before switching to the byte pipe
upgrade_init(Headers, Req, BackendClient) ->
    %% fetch raw sockets and buffers
    {Server={TransVeg,SockVeg}, BufVeg, _NewClient} = vegur_client:raw_socket(BackendClient),
    {Client={TransCow,SockCow}, BufCow, Req2} = vegur_utils:raw_cowboyku_sockbuf(Req),
    %% Send the response to the caller
    Headers1 = vegur_client:headers_to_iolist(
                 vegur_headers:upgrade_response_headers(Headers)
                ),
    TransCow:send(SockCow,
                  [<<"HTTP/1.1 101 Switching Protocols\r\n">>,
                   Headers1, <<"\r\n">>,
                   BufVeg]),
    %% Flush leftover buffer data from the client, if any
    TransVeg:send(SockVeg, BufCow),
    %% Hand in stuff that's ready to be used
    {Client, Server, Req2}.


-spec relay(Code, Status, Headers, Req, Client) ->
                   {ok, Req, Client} |
                   {error, Blame, Error, Req} when
      Code :: pos_integer(),
      Status :: binary(),
      Headers :: [{binary(), binary()}]|[],
      Req :: cowboyku_req:req(),
      Client :: vegur_client:client(),
      Error :: any(),
      Blame :: error_blame().
relay(Code, Status, HeadersRaw, Req, Client) ->
    %% Dispatch data from vegur_client down into the cowboyku connection, either
    %% in batch or directly.
    {Headers, Req1} = case connection_type(Code, Req, Client) of
        {keepalive, Req0} ->
            vegur_utils:add_interface_headers(
                downstream,
                vegur_headers:add_connection_keepalive_header(
                  vegur_headers:response_headers(HeadersRaw)
                 ),
                Req0
            );
        {close, Req0} ->
            vegur_utils:add_interface_headers(
                downstream,
                vegur_headers:add_connection_close_header(
                  vegur_headers:response_headers(HeadersRaw)
                 ),
                Req0
            )
    end,
    case vegur_client:body_type(Client) of
        {content_size, N} when N =< ?UPSTREAM_BODY_BUFFER_LIMIT ->
            relay_full_body(Code, Status, Headers, Req1, Client);
        {content_size, N} ->
            relay_stream_body(Code, Status, Headers, N, fun stream_body/2, Req1, Client);
        stream_close -> % unknown content-lenght, stream until connection close
            relay_stream_body(Code, Status, Headers, undefined, fun stream_close/2, Req1, Client);
        chunked ->
            relay_chunked(Code, Status, Headers, Req1, Client);
        no_body ->
            relay_no_body(Code, Status, Headers, Req1, Client)
    end.

%% There is no body to relay
relay_no_body(_Code, Status, Headers, Req, Client) ->
    Req1 = respond(Status, Headers, <<>>, Req),
    {ok, Req1, backend_close(Client)}.

%% The entire body is known and we can pipe it through as is.
relay_full_body(Code, Status, Headers, Req, Client) ->
    case wait_for_body(Code, Req) of
        {dont_wait, Req1} -> % stream nothing
            {content_size, N} = vegur_client:body_type(Client),
            relay_stream_body(Code, Status, Headers, N, fun stream_nothing/2, Req1, Client);
        {wait, Req1} -> % send the whole thing at once when ready
            case vegur_client:response_body(Client) of
                {ok, Body, Client2} ->
                    Req2 = respond(Status, Headers, Body, Req1),
                    {ok, Req2, backend_close(Client2)};
                {error, Error} ->
                    backend_close(Client),
                    {error, downstream, Error, Req1}
            end
    end.

%% The body is large and may need to be broken in multiple parts. Send them as
%% they come.
-spec relay_stream_body(Code, Status, Headers, Size, StreamFun, Req, Client) ->
    {ok, Req, Client} | {error, Blame, Reason, Req} when
    Code :: non_neg_integer(),
    Status :: binary(),
    Headers :: [{binary(),binary()}],
    Size :: undefined | non_neg_integer(),
    StreamFun :: fun(({module(),Sock::term()}, Client) -> {error, Reason} | {error, Blame, Reason} | {ok, Client}),
    Req :: cowboyku_req:req(),
    Client :: vegur_client:client(),
    Blame :: error_blame(),
    Reason :: term().
relay_stream_body(Code, Status, Headers, Size, StreamFun, Req, Client) ->
    Req2 = relay_stream_body_init(Code, Status, Headers, Size, StreamFun, Req, Client),
    %% We use the process dictionary to carry around a buffer of
    %% data read from cowboyku's client socket, necessary to detect connections
    %% that closed. In such cases, it is possible that data makes it to us
    %% and requires to be buffered to be served better.
    buffer_init(),
    try cowboyku_req:reply(Status, Headers, Req2) of % --ignore dialyzer
        {ok, Req3} ->
            Buf = buffer_clear(),
            {ok,
             vegur_utils:append_to_cowboyku_buffer(Buf,Req3),
             backend_close(Client)}
    catch
        {stream_error, Blame, Error} ->
            buffer_clear(),
            backend_close(Client),
            {error, Blame, Error, Req2}
    end.

%% Prepare the body to be streamed.
%% Use cowboyku's partial response delivery to stream contents.
%% We use exceptions (throws) to detect bad transfers and close
%% both connections when this happens.
relay_stream_body_init(Code, _Status, _Headers, Size, StreamFun, Req, Client) ->
    %% Check whether we need to wait for a body at all
    {FinalFun, Req1} = case wait_for_body(Code, Req) of
        {wait, Req0} -> {StreamFun, Req0};
        {dont_wait, Req0} -> {fun stream_nothing/2, Req0}
    end,
    %% Wrap the streaming function to throw exceptions so we
    %% escape cowboyku's scope
    Fun = fun(Socket, Transport) ->
        case FinalFun({Transport,Socket}, Client) of
            {ok, Client2} ->
                {ok, Client2};
            {error, Blame, Reason} ->
                throw({stream_error, Blame, Reason})
        end
    end,
    %% Add the function to the Req object
    case Size of
        undefined -> cowboyku_req:set_resp_body_fun(Fun, Req1); % end on close
        _ -> cowboyku_req:set_resp_body_fun(Size, Fun, Req1)    % end on size
    end.

relay_chunked(Code, Status, Headers, Req, Client) ->
    {Version, Req2} = cowboyku_req:version(Req),
    relay_chunked(Version, Code, Status, Headers, Req2, Client).

relay_chunked('HTTP/1.1', Code, Status, Headers, Req, Client) ->
    %% This is a special case. We stream pre-delimited chunks raw instead
    %% of using cowboyku, which would have to recalculate and re-delimitate
    %% sizes all over after we parsed them first. We save time by just using
    %% raw chunks.
    {RawSocket, Req2} = relay_chunked_init(Status, Headers, Req),
    case wait_for_body(Code, Req2) of
        {dont_wait, Req3} ->
            {ok, Req3, backend_close(Client)};
        {wait, Req3} ->
            buffer_init(),
            case stream_chunked(RawSocket, Client) of
                {ok, Client2} ->
                    Buf = buffer_clear(),
                    {ok,
                     vegur_utils:append_to_cowboyku_buffer(Buf,Req3),
                     backend_close(Client2)};
                {error, Blame, Error} -> % uh-oh, we died during the transfer
                    buffer_clear(),
                    backend_close(Client),
                    {error, Blame, Error, Req3}
            end
    end;
relay_chunked('HTTP/1.0', Code, Status, Headers, Req, Client) ->
    %% This case means that we're forwarding chunked encoding to an
    %% older client that doesn't support it. The way around this is to
    %% stream the data as-is, but use no `content-length' header *AND* a
    %% `connection: close' header to implicitly delimit the request
    %% as streaming unknown-size HTTP.
    relay_stream_body(Code, Status,
                      vegur_headers:delete_transfer_encoding_header(Headers),
                      undefined, fun stream_unchunked/2, Req, Client).


relay_chunked_init(Status, Headers, Req) ->
    %% Set the chunked reply type, then we can just stream later on.
    %% If we didn't use that first call, we'd need to handle headers
    %% and whatnot by hand, and call reply with the custom chunked
    %% encoding headers. This is more or less just a shortcut
    %% to get the initial chunked headers on the line, then we're
    %% free to take over the body.
    {ok, Req2} = cowboyku_req:chunked_reply(Status, Headers, Req),
    {RawSocket, Req3} = vegur_utils:raw_cowboyku_socket(Req2),
    {RawSocket, Req3}.


%% Chunks from backend -> frontend
stream_chunked({Transport,Sock}=Raw, Client) ->
    %% Fetch chunks one by one (including length and line-delimitation)
    %% and forward them over the raw socket.
    case vegur_client:stream_chunk(Client) of
        {ok, Data, Client2} ->
            case check_and_send(Transport, Sock, Data) of
                ok -> stream_chunked(Raw, Client2);
                {error, Reason} -> {error, upstream, Reason}
            end;
        {more, _Len, Data, Client2} ->
            case check_and_send(Transport, Sock, Data) of
                ok -> stream_chunked(Raw, Client2);
                {error, Reason} -> {error, upstream, Reason}
            end;
        {done, Data, Client2} ->
            Transport:send(Sock, Data),
            {ok, backend_close(Client2)};
        {error, Reason} ->
            backend_close(Client),
            {error, downstream, Reason}
    end.

%% Chunks from backend -> frontend
stream_unchunked({Transport,Sock}=Raw, Client) ->
    %% Fetch chunks one by one (excluding length and line-delimitation)
    %% and forward them over the raw socket.
    case vegur_client:stream_unchunk(Client) of
        {ok, Data, Client2} ->
            case check_and_send(Transport, Sock, Data) of
                ok -> stream_unchunked(Raw, Client2);
                {error, Reason} -> {error, upstream, Reason}
            end;
        {more, _Len, Data, Client2} ->
            case check_and_send(Transport, Sock, Data) of
                ok -> stream_unchunked(Raw, Client2);
                {error, Reason} -> {error, upstream, Reason}
            end;
        {done, Data, Client2} ->
            Transport:send(Sock, Data),
            {ok, backend_close(Client2)};
        {error, Reason} ->
            backend_close(Client),
            {error, downstream, Reason}
    end.

%% Deal with the transfer of a large or chunked request body by
%% going from a cowboyku stream to raw vegur_client requests
%% frontend -> backend
stream_request(Req, Client) ->
    %% Initialize the request with two empty buffers (one for the
    %% frontend connection, one for the backend connection), and
    %% figure out, assuming 1s polling intervals in cowboyku, how many
    %% of them we can do. By default we go to 55 seconds, meaning
    %% we will try as many as 55 polling sequences on both ends.
    stream_request(<<>>, Req, Client, <<>>, ?MODULE:reps_left()).

%% The idea is to enter a loop with two buffers and be able to
%% just shuttle data between both ends at once:
%%
%%  Frontend <--f--> Vegur <--b--> Backend
%%
%% All operations we do comprise three potential actions:
%%
%% 1. poll the backend connection (b) to detect for termination.
%%    This isn't required on the loopback interface, but is needed
%%    anywehre else. To properly detect a connection termination on
%%    a FIN packet (RST seems to work okay) we need to read everything
%%    that was standing in the receive buffer.
%%
%%    This read buffer is transfered to the buffer for (b), initialized
%%    here. The subtle bit in the code flow below is that detection of
%%    termination on a `recv' is reflected in the next `send'. This is
%%    crucial because the buffer for (b) might be full, at which point
%%    we will rely solely on the slower `send' operation to find issues,
%%    if any (say using `RST').
%% 2. poll the frontend connection (f) to read data from it. This data
%%    can then be sent *if* the backend connection is still alive. If no
%%    data is found, we need to try for more.
%% 3. Send the data as soon as possible, if the connection to the backend
%%    (b) is still open
%%
%% This is repeated for every packet we have, in a loop.
%%
%% It is important to note that in order to have the shortest interval
%% possible between a connection termination and its detection when sending
%% data, the code structure has to be twisted a bit so that step 1 happens
%% right before step 3. So instead of going with the simplest path:
%%
%%   1      -> 2      -> 3    -> 1      -> ...
%%   poll b -> poll f -> send -> poll b -> ...
%%   0s     -> 1s     -> 0s   -> 0s     -> ...
%%
%% Which may accidentally delay the time it takes to find a problem when sending
%% data by up to 1s, we instead use a buffer that may or may not be empty
%% representing data to send, allowing us to do:
%%
%%   1       -> 3    -> 2     -> 1      -> ...
%%   poll b -> send -> poll f -> poll b -> ...
%%   0s     -> 0s   -> 1s     -> 0s     -> ...
%%
%% Resulting in faster response times for errors and invalid codes given they
%% will be followed by a closed connection.
stream_request(_Buffer, _Req, _Client, _DownBuffer, 0) ->
    {error, upstream, timeout};
stream_request(Buffer, Req, Client, DownBuffer, N) ->
    %% Buffer the data so that if the connection is
    %% interrupted, we can read the response (if any) before communicating
    %% the closed connection to the client
    NewDownBuffer = check_downstream(Client, DownBuffer),
    %% Send data once, if any
    case maybe_raw_request(Buffer, Client) of
        {ok, Client2} ->
            %% Buffer again
            case cowboyku_req:stream_body(Req) of % hardcoded 1s
                {done, Req2} ->
                    {done, Req2, vegur_client:append_to_buffer(NewDownBuffer,Client2)};
                {ok, Data, Req2} ->
                    stream_request(Data, Req2, Client2, NewDownBuffer, ?MODULE:reps_left());
                {error, timeout} ->
                    %% Reset the buffer
                    stream_request(<<>>, Req, Client2, NewDownBuffer, N-1);
                {error, Err} ->
                    {error, upstream, Err}
            end;
        {error, closed} when byte_size(DownBuffer) > 0 ->
            %% we have a buffer accumulated, it's likely an early response came
            %% while streaming the body. We must however force the connection
            %% to be closed because we won't wait until the full body is read.
            Req2 = vegur_utils:mark_cowboyku_close(Req),
            {done, Req2, vegur_client:append_to_buffer(NewDownBuffer,Client)};
        {error, Err} ->
            {error, downstream, Err}
    end.

reps_left() ->
    %% if changing for a config value rather than hardcoded, remember
    %% to set a lower boundary to 1 rep.
    55.

maybe_raw_request(<<>>, Client) ->
    %% Nothing to send, but behave as if we did something
    {ok, Client};
maybe_raw_request(Data, Client) ->
    %% Actually send data
    vegur_client:raw_request(Data, Client).

%% Cowboyku also allows to decode data further after one pass, say if it
%% was gzipped or something. For our use cases, we do not care about this
%% as we relay the information as-is, so this function does nothing.
decode_identity(Data) ->
    {ok, Data}.

%% Custom decoder for Cowboyku that will allow to stream data without modifying
%% it, in bursts, directly to the backend without accumulating it in memory.
decode_raw(Data, {Streamed, Total}) when Streamed + byte_size(Data) < Total ->
    %% Still a lot to go, we return it all as a frame
    {ok, Data, <<>>, {Streamed+iolist_size(Data), Total}};
decode_raw(Data, {Streamed, Total}) ->
    %% Last batch, but we may have more than we asked for.
    Size = Total-Streamed,
    <<Data2:Size/binary, Rest/binary>> = Data,
    {done, Data2, Total, Rest}.

%% Custom decoder for Cowboyku that will allow to return chunks in streams while
%% still giving us a general idea when a chunk begins and ends, and when the
%% entire request is cleared. Can deal with partial chunks for cases where
%% the user sends in multi-gigabyte chunks to mess with us.
%% frontend -> backend usage.
decode_chunked(Data, {InitCont, Cont, Total}) ->
    case vegur_chunked:stream_chunk(Data, Cont) of
        {done, Buf, Rest} ->
            %% Entire request is over
            {done, Buf, Total+iolist_size(Buf), Rest};
        {error, Reason} ->
            {error, Reason};
        {chunk, Buf, Rest} ->
            %% Chunk is done, but more to come
            {ok, Buf, Rest, {InitCont, InitCont, Total+iolist_size(Buf)}};
        {more, _Len, Buf, Cont2} ->
            %% Not yet done on the current chunk, but keep going.
            {ok, Buf, <<>>, {InitCont, Cont2, Total}}
    end.

respond(Status, Headers, Body, Req) ->
    {ok, Req1} = cowboyku_req:reply(Status, Headers, Body, Req),
    Req1.

%% backend -> frontend
stream_body({Transport,Sock}=Raw, Client) ->
    %% Stream the body until as much data is sent as there
    %% was in its content-length initially.
    case vegur_client:stream_body(Client) of
        {ok, Data, Client2} ->
            case check_and_send(Transport, Sock, Data) of
                ok -> stream_body(Raw, Client2);
                {error, Reason} -> {error, upstream, Reason}
            end;
        {done, Client2} ->
            {ok, vegur_client:set_stats(Client2)};
        {error, Reason} ->
            {error, downstream, Reason}
    end.

%% backend -> frontend
stream_close({Transport,Sock}=Raw, Client) ->
    %% Stream the body until the connection is closed.
    case vegur_client:stream_close(Client) of
        {ok, Data, Client2} ->
            case check_and_send(Transport, Sock, Data) of
                ok -> stream_close(Raw, Client2);
                {error, Reason} -> {error, upstream, Reason}
            end;
        {done, Client2} ->
            {ok, vegur_client:set_stats(Client2)};
        {error, Reason} ->
            {error, downstream, Reason}
    end.

stream_nothing(_Raw, Client) ->
    {ok, Client}.

%% We should close the connection whenever we get an Expect: 100-Continue
%% that got answered with a final status code.
connection_type(Code, Req, Client) ->
    {Cont, _} = cowboyku_req:meta(continue, Req, []),
    %% If we haven't received a 100 continue to forward after having
    %% received an expect AND this is a final status, then we should
    %% close the connection.
    case Cont =:= continue andalso Code >= 200 of
        true ->
            {close, Req};
        false ->
            %% Honor the client's decision, except if the response has no
            %% content-length, in which case closing is mandatory
            case vegur_client:body_type(Client) of
                stream_close ->
                    case wait_for_body(Code, Req) of
                        {dont_wait, Req2} ->
                            {cowboyku_req:get(connection, Req2), Req2};
                        {wait, Req2} ->
                            {close, Req2}
                    end;
                chunked ->
                    %% Chunked with an HTTP/1.0 client gets turned to a close
                    %% to allow proper data streaming.
                    case cowboyku_req:version(Req) of
                        {'HTTP/1.0', Req2} -> {close, Req2};
                        {'HTTP/1.1', Req2} -> {cowboyku_req:get(connection, Req2), Req2}
                    end;
                _ ->
                    {cowboyku_req:get(connection, Req), Req}
            end
    end.

%% @doc This function will return `dont_wait' if we know the transfer-length of
%% the message will be 0 as per the RFC, in cases such as HEAD requests and
%% specific status codes.
%% `wait' will be returned in all other cases where the body length may
%% be non-0 depending on the request/response.
wait_for_body(204, Req) -> {dont_wait, Req};
wait_for_body(304, Req) -> {dont_wait, Req};
wait_for_body(_, Req) ->
    case cowboyku_req:method(Req) of
        {<<"HEAD">>, Req1} -> {dont_wait, Req1};
        {_, Req1} -> {wait, Req1}
    end.

backend_close(Client) ->
    case get(should_keepalive) of
        true ->
            case vegur_client:connection(Client) of
                keepalive ->
                    put(reuse, Client),
                    Client; % do not set delta here -- stats can be fetched after reqs are done
                close ->
                    erase(reuse),
                    vegur_client:close(Client)
            end;
        _ ->
            vegur_client:close(Client)
    end.

%% When sending data in passive mode, it is usually impossible to be notified
%% of connections being closed. For this to be done, we need to poll the socket
%% we're writing to.
%%
%% This function does it in a protected way that uses a buffer to avoid
%% breaking pipelining. If the buffer is full, data is left on the line
%% and detection is implicitly disabled. The buffer is stored in the
%% process dictionary as a side-effect of using cowboy functionality
%% that does not let us carry state around.
%%
%% There is no expectation that data sent after a successful check actually
%% makes it to the client -- this is only done to detect if FIN packets have
%% ever been sent on the port so that the connection can be closed from our
%% side to. Not doing this creates half-closed connections where we can
%% write to the front-end but they can't write back, forever.
check_and_send(Transport, Sock, Data) ->
    case check(Transport, Sock) of
        ok -> Transport:send(Sock, Data);
        Err -> Err
    end.

check(Transport, Sock) ->
    %% Read data, wait 0ms. This function is messy and cooperates with
    %% relay_stream_body/6 and relay_chunked/5 to carry around a limited
    %% buffer of unexpected data coming from the client.
    case buffer_size() >= ?UPSTREAM_BODY_BUFFER_LIMIT of
        true -> % no check after buffer is full, let the kernel handle it.
            ok;
        false ->
            case Transport:recv(Sock, 0, 0) of
                {error, timeout} -> % connection still alive, but no data
                    ok;
                {error, Reason} ->
                    {error, Reason};
                {ok, Data} ->
                    buffer_append(Data),
                    buffer_size() >= ?UPSTREAM_BODY_BUFFER_LIMIT andalso
                        error_logger:info_msg("mod=vegur_proxy at=check message=upstream_buffer_full\n"),
                    ok
            end
    end.

%% When uploading data from the client to the back-end server, there is a
%% possibility that the transfer will be long and slow, but that the server
%% interjects with an early response (such as a 3xx, 4xx, or 5xx status),
%% and then promptly closing the connection.
%%
%% The problem is that if the upload is particularly long, we may eventually
%% find that the connection was broken while sending. At this point, the
%% data that was lingering in the receive buffer from the server is no longer
%% available, and we might report the connection as interrupted, while it was
%% acceptable to have a response comming back.
%%
%% This function is used to compare buffers from downstream and fetch data from
%% the line, so that in the eventuality the case above happens, we still have
%% the data available to us, and can figure out whether it was an interruption,
%% or a good response. In the latter case, we can then forward that response
%% to the caller and let it figure out how to deal with it.
check_downstream(_, Buffer) when byte_size(Buffer) >= ?DOWNSTREAM_BODY_BUFFER_LIMIT ->
    %% No check after buffer is full, we'll just check when we're done with
    %% the downstream transfer
    Buffer;
check_downstream(Client, Buffer) ->
    {Transport, Sock} = vegur_client:borrow_socket(Client),
    case Transport:recv(Sock, 0, 0) of
        {error, _} ->
            %% Timeouts or connection errors do not end up changing the buffer;
            %% let the caller find errors if need be.
            Buffer;
        {ok, Data} ->
            NewBuf = <<Buffer/binary, Data/binary>>,
            byte_size(NewBuf)  >= ?DOWNSTREAM_BODY_BUFFER_LIMIT andalso
                error_logger:info_msg("mod=vegur_proxy at=check message=downstream_buffer_full\n"),
            NewBuf
    end.

%%% Buffer management functions

%% Creates a new empty buffer for pipelined requests
buffer_init() -> put(vegur_pipeline_buffer, <<>>).

%% Removes data from the pipelined requests buffer, and
%% returns what was in there
buffer_clear() -> erase(vegur_pipeline_buffer).

%% Returns the size of the buffer, in bytes
buffer_size() -> byte_size(get(vegur_pipeline_buffer)).

%% Adds an arbitrary piece of binary data at the end of
%% the existing buffer value.
buffer_append(Data) when is_binary(Data) ->
    Buf = get(vegur_pipeline_buffer),
    put(vegur_pipeline_buffer, <<Buf/binary, Data/binary>>).
