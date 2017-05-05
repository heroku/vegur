%% Copyright (c) 2012-2013, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%
%% Copyright (c) 2013-2015, Heroku <routing-feedback@heroku.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


%% @private
-module(vegur_client).

-export([init/1]).
-export([state/1]).
-export([transport/1]).
-export([close/1]).

-export([connect/4]).
-export([connect/5]).
-export([raw_request/2]).
-export([request/3]).
-export([request/4]).
-export([request/5]).
-export([response/1]).
-export([response_body/1]).
-export([next_chunk/1]).
-export([next_chunk/2]).
-export([skip_body/1]).
-export([stream_status/1]).
-export([stream_headers/1]).
-export([stream_header/1]).
-export([stream_body/1]).
-export([stream_close/1]).
-export([stream_chunk/1]).
-export([stream_unchunk/1]).
-export([buffer_data/3]).
-export([append_to_buffer/2]).

-export([body_type/1]).
-export([connection/1]).
-export([version/1]).

-export([headers_to_iolist/1]).
-export([request_to_headers_iolist/6]).
-export([request_to_iolist/6]).
-export([raw_socket/1]).
-export([borrow_socket/1]).
-export([auth_header/1]).

-export([set_stats/1]).
-export([set_delta/1]).
-export([byte_counts/1]).
-export([log/1]).
-export([reset_log/1]).

-define(REASON_MISSING, <<"">>). %% "EBADDEVELOPER"

-record(client, {
          state = wait :: wait | request | response | response_body | raw,
          opts = [] :: [any()],
          socket = undefined :: undefined | inet:socket(),
          transport = undefined :: module() | tuple(), % tuple for tuple calls
          connect_timeout = vegur_utils:config(downstream_connect_timeout) :: timeout(),
          first_read_timeout = timer:seconds(vegur_utils:config(downstream_first_read_timeout)) :: timeout() | undefined,
          read_timeout = timer:seconds(vegur_utils:config(idle_timeout)) :: timeout(),
          buffer = <<>> :: binary(),
          connection = keepalive :: keepalive | close,
          version = 'HTTP/1.1' :: cowboyku:http_version(),
          status = undefined :: 100..999 | undefined, % can be undefined before parsing
          response_body = undefined :: chunked | undefined | non_neg_integer(),
          bytes_sent :: non_neg_integer() | undefined, % Bytes sent downstream
          bytes_recv :: non_neg_integer() | undefined, % Bytes recv from downstream
          bytes_sent_offset=0 :: integer(), % Bytes sent downstream modification
          bytes_recv_offset=0 :: integer(), % Bytes recv from downstream modification
          first_packet_recv :: undefined | erlang:timestamp(),
          last_packet_recv :: undefined | erlang:timestamp(),
          first_packet_sent :: undefined | erlang:timestamp(),
          last_packet_sent :: undefined | erlang:timestamp(),
          log :: vegur_req_log:request_log()
}).

-type client() :: #client{}.
-export_type([client/0]).

%% @doc initialize a disconnected client, to be used to set up a connection
%% at a later point. The internal client log is initialized here.
-spec init([any()]) -> {ok, client()}.
init(Opts) ->
    {ok, #client{opts=Opts, log=vegur_req_log:new(os:timestamp())}}.

%% @doc introspection function returning the state of the client. Can be used
%% to retrieve some level of state about the progress of a request/response
%% cycle.
-spec state(client()) -> wait | request | response | response_body | raw.
state(#client{state=State}) ->
    State.

%% @deprecated
%% @doc Returns the ranch transport information for the client.
%% Ambiguous in usage and meaning, does not carry buffering.
%% Consider using `raw_socket/1' or `borrow_socket/1' instead.
-spec transport(client()) -> {error, notconnected} | {ok, module(), term()}.
transport(#client{socket=undefined}) ->
    {error, notconnected};
transport(#client{transport=Transport, socket=Socket}) ->
    {ok, Transport, Socket}.

%% @doc Close the connection held open by the client, if any.
-spec close(client()) -> client().
close(#client{socket=undefined}=Client) ->
    Client;
close(#client{transport=Transport, socket=Socket, log=Log}=Client) ->
    NewClient=set_stats(Client),
    Transport:close(Socket),
    NewClient#client{socket=undefined,
                     log=vegur_req_log:stamp(client_close, Log)};
close({Client=#client{}, _Continuation}) ->
    %% Used as a wrapper for streamed connections
    close(Client).

%% @doc establish a new connection from a disconnected client.
%% See `connect/4'.
-spec connect(module(), binary() | inet:ip_address() | inet:hostname(),
              inet:port_number(), timeout(), client()) ->
                 {ok, client()} | {error, term()}.
connect(Transport, Host, Port, Timeout, Client) ->
    connect(Transport, Host, Port, Client#client{connect_timeout=Timeout}).


%% @doc establish a new connection from a disconnected client.
%% Inserts the `client_connect' event in the log.
-spec connect(module(), inet:ip_address() | inet:hostname(), inet:port_number(),
              client()) -> {ok, client()} | {error, term()}.
connect(Transport, Host, Port, Client)
        when is_binary(Host) ->
    connect(Transport, binary_to_list(Host), Port, Client);
connect(Transport, Host, Port,
        Client=#client{state=wait, opts=Opts, connect_timeout=Timeout, log=Log})
        when is_atom(Transport),
            (is_list(Host) orelse is_tuple(Host)),
            is_integer(Port) ->
    try Transport:connect(Host, Port, Opts, Timeout) of
        {ok, Socket} ->
            {ok, Client#client{state=request,
                               socket=Socket,
                               transport=Transport,
                               log=vegur_req_log:stamp(client_connect, Log)}};
        {error, _} = Err -> Err
    catch
        error:Reason -> {error, Reason}
    end.

%% @doc Sends bytes from a request directly to a remote end. If a response from
%% a prior request was waiting in the buffers, that response is skipped.
-spec raw_request(iodata(), client()) -> {ok, client()} | {error, term()}.
raw_request(Data, Client=#client{state=response_body}) ->
    {done, Client2} = skip_body(Client),
    raw_request(Data, Client2);
raw_request(Data, Client=#client{
        state=request, socket=Socket, transport=Transport}) ->
    case Transport:send(Socket, Data) of
        ok ->
            {ok, set_stats(stamp_sent(Client))};
        {error, _} = Err ->
            Err
    end.

%% @doc equivalent to `request(Method, URL, [], <<>>, Client)'.
request(Method, URL, Client) ->
    request(Method, URL, [], <<>>, Client).

%% @doc equivalent to `request(Method, URL, Headers, <<>>, Client)'.
request(Method, URL, Headers, Client) ->
    request(Method, URL, Headers, <<>>, Client).

%% @doc Sends an HTTP request by formatting headers and attaching the body with
%% them. Only expects clients which are not connected, or clients which are
%% connected following a keepalive request sent with this function.
%% Do not pass in a pre-connected client here as you risk getting connection
%% leaks.
-spec request(Method, URL, [{HeaderName, HeaderVal}], Body, client()) ->
        {ok, client()} | {error, term()} when
    Method :: iodata(),
    URL :: binary(),
    HeaderName :: binary(),
    HeaderVal :: binary(),
    Body :: iodata().
request(Method, URL, Headers, Body, Client=#client{state=response_body}) ->
    {done, Client2} = skip_body(Client),
    request(Method, URL, Headers, Body, Client2);
request(Method, URL, Headers, Body, Client=#client{
        state=State, version=Version})
        when State =:= wait; State =:= request ->
    {Transport, FullHost, Host, Port, Path} = parse_url(URL),
    {ok, Client2} = case State of
        wait -> connect(Transport, Host, Port, Client);
        request -> {ok, Client}
    end,
    Data = request_to_iolist(Method, Headers, Body, Version, FullHost,
                                     Path),
    raw_request(Data, Client2).

%% @doc Takes a request's data (headers, method, body, HTTP version) and
%% generates the headers that will be required to be successful. The body
%% must be passed or known in order to set a given content length.
%% A caveat is that no length is generated for a chunked transfer; remember
%% to set the transfer encoding yourself.
-spec request_to_headers_iolist(Method, [{HeaderName, HeaderVal}], Body,
                                cowboyku:http_version(), FullHost, Path) -> iodata() when
    Method :: iodata(),
    HeaderName :: binary(),
    HeaderVal :: binary(),
    Body :: iodata() | {stream, non_neg_integer() | chunked},
    FullHost :: iodata(),
    Path :: iodata().
request_to_headers_iolist(Method, Headers, Body, Version, FullHost, Path) ->
    VersionBin = atom_to_binary(Version, latin1),
    %% @todo do keepalive too, allow override...
    Headers2 = [{<<"Host">>, FullHost} | Headers],
    ContentLength = content_length_header(Method, Body),
    HeadersData = headers_to_iolist(Headers2++ContentLength),
    [Method, <<" ">>, Path, <<" ">>, VersionBin, <<"\r\n">>,
     HeadersData, <<"\r\n">>].

%% @private
content_length_header(_, {stream, 0}) -> [];
content_length_header(_, {stream, chunked}) -> [];
content_length_header(_, {stream, Length}) when is_integer(Length) ->
    [{<<"Content-Length">>, integer_to_list(Length)}];
content_length_header(Method, <<>>)
  when Method =:= <<"GET">>;
       Method =:= <<"HEAD">> ->
    [];
content_length_header(_, Body) ->
    [{<<"Content-Length">>, integer_to_list(iolist_size(Body))}].

%% @doc Changes a set of header lists into a Header-Cased set of HTTP headers
%% with proper line terminations
-spec headers_to_iolist([{binary(), binary()}]) -> iodata().
headers_to_iolist(Headers) ->
    [[cowboyku_bstr:capitalize_token(Name), <<": ">>, Value, <<"\r\n">>] || {Name, Value} <- Headers].

%% @doc Takes a the components of a request and returns an `iodata()' that
%% represents the full request and can be sent over the network.
-spec request_to_iolist(Method, [{HeaderName, HeaderVal}], Body,
                        cowboyku:http_version(), FullHost, Path) -> iodata() when
    Method :: iodata(),
    HeaderName :: binary(),
    HeaderVal :: binary(),
    Body :: iodata() | {stream, non_neg_integer() | chunked},
    FullHost :: iodata(),
    Path :: iodata().
request_to_iolist(Method, Headers, Body, Version, FullHost, Path) ->
    [request_to_headers_iolist(Method, Headers, Body, Version, FullHost, Path),
     Body].

%% @doc Allows to extract the socket and pending buffer from the client.
%% The client's own buffer is removed, the state is changed to `raw', which
%% makes it mostly unusable with other requests.
%% This allows to take over HTTP functionality in some cases, without
%% necessarily dropping the request logs and other useful data structures.
%% By taking over the connection, the caller also takes charge of ensuring
%% multiple requests cannot be sent at once, or that trailing bodies are
%% not left dangling on the line. The vegur client is not going to be reused
%% for HTTP data.
%% Use at your own risk.
-spec raw_socket(client()) -> {{module(), term()}, iodata(), client()}.
raw_socket(Client=#client{transport=T, socket=S, buffer=Buf}) ->
    {{T,S}, Buf, Client#client{buffer= <<>>, state=raw}}.

%% @doc Less destructive form of `raw_socket/1': this form is intended to
%% be used for temporary or transient use that will not read from the socket
%% (as the buffer is kept internal to the client). This function can be useful
%% to sneak in data over the line (for example TCP PROXY protocol, or manually
%% negotiating a 100-Continue), but it is expected that the connection is still
%% owned and managed by vegur_client. Therefore, the caller must be careful
%% not to break these promises. Safe usage tends to include obtaining metadata
%% from the socket, and possibly flushing data down the line.
%% Use at your own risk.
-spec borrow_socket(client()) -> {module(), term()}.
borrow_socket(#client{transport=T, socket=S}) ->
    {T,S}.

%% @private
parse_url(<< "https://", Rest/binary >>) ->
    parse_url(Rest, ranch_ssl);
parse_url(<< "http://", Rest/binary >>) ->
    parse_url(Rest, ranch_tcp);
parse_url(URL) ->
    parse_url(URL, ranch_tcp).

%% @private
parse_url(URL, Transport) ->
    case binary:split(URL, <<"/">>) of
        [Peer] ->
            {Host, Port} = parse_peer(Peer, Transport),
            {Transport, Peer, Host, Port, <<"/">>};
        [Peer, Path] ->
            {Host, Port} = parse_peer(Peer, Transport),
            {Transport, Peer, Host, Port, [<<"/">>, Path]}
    end.

%% @private
parse_peer(Peer, Transport) ->
    case binary:split(Peer, <<":">>) of
        [Host] when Transport =:= ranch_tcp ->
            {binary_to_list(Host), 80};
        [Host] when Transport =:= ranch_ssl ->
            {binary_to_list(Host), 443};
        [Host, Port] ->
            {binary_to_list(Host), list_to_integer(binary_to_list(Port))}
    end.

%% @doc Fetches the response headers to the current request. If the body of
%% a prior request was left on the line, this tries to skip it.
-spec response(client()) ->
    {ok, Status, StatusStr, [{HeaderName, HeaderVal}], client()} | {error, term()} when
      Status :: 100..999,
      StatusStr :: binary(),
      HeaderName :: binary(),
      HeaderVal :: binary().
response(Client=#client{state=response_body}) ->
    case skip_body(Client) of
        {done, Client2} ->
            response(set_stats(Client2));
        {error, Reason} ->
            {error, Reason}
    end;
response(Client=#client{state=request}) ->
    case stream_status(Client) of
        {ok, Status, StatusStr, Client2} ->
            case stream_headers(Client2) of
                {ok, Headers, Client3} ->
                    {ok,
                     Status,
                     <<(integer_to_binary(Status))/binary, " ", StatusStr/binary>>,
                     Headers,
                     set_stats(Client3)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Returns the type of body of the response to the request.
-spec body_type(client()) -> chunked | no_body | {content_size, non_neg_integer()} | stream_close.
body_type(#client{response_body=chunked}) -> chunked;
body_type(#client{state=request, response_body=0}) -> no_body;
body_type(#client{state=response_body, response_body=Length}) -> {content_size, Length};
body_type(#client{state=request, status=204, response_body=undefined}) -> no_body;
body_type(#client{state=request, status=304, response_body=undefined}) -> no_body;
body_type(#client{state=request}) -> stream_close.

%% @doc Returns the HTTP connection type
-spec connection(client()) -> keepalive | close.
connection(#client{connection=Connection}) -> Connection.

%% @doc Returns the HTTP version of the response (if any), otherwise always
%% returns the default of `HTTP/1.1'.
-spec version(client()) -> cowboyku:http_version().
version(#client{version=Version}) -> Version.

%% @doc returns the full HTTP response body. If the response is chunked, the
%% chunked content is returned as-is (not un-chunked). This is because this is
%% a more practical behaviour for a proxy that does not intend to reformat
%% response bodies. Streaming interfaces to content-body handling are likely
%% more appropriate for a proxy, though.
-spec response_body(client()) -> {ok, iodata(), client()} | {error, term()}.
response_body(Client=#client{response_body=chunked}) ->
    response_body_chunk(Client, <<>>);
response_body(Client=#client{state=response_body}) ->
    response_body_loop(Client, <<>>);
response_body(Client=#client{state=request, connection=close}) ->
    response_body_close(Client, <<>>).

%% @private
response_body_loop(Client, Acc) ->
    case stream_body(Client) of
        {ok, Data, Client2} ->
            response_body_loop(Client2, << Acc/binary, Data/binary >>);
        {done, Client2} ->
            {ok, Acc, set_stats(Client2)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc WARNING: we only support chunked reading where we get back
%% a bunch of raw chunks. As a proxy, we have no interest into
%% decoding chunks for humans, just for the next hop.
response_body_chunk(Client, Acc) ->
    case next_chunk(Client) of
        {ok, Data, Client2} ->
            response_body_chunk(Client2, [Acc, Data]);
        {done, Buf, Client2} ->
            {ok, [Acc, Buf], set_stats(Client2)};
        {error, Reason} ->
            {error, Reason}
    end.

next_chunk(Client) -> next_chunk(Client, undefined).

next_chunk(Client=#client{buffer=Buffer}, Cont) ->
    case vegur_chunked:next_chunk(Buffer, Cont) of
        {done, Buf, Rest} ->
            {done, Buf, Client#client{buffer=Rest, response_body=undefined}};
        {chunk, Buf, Rest} ->
            {ok, Buf, Client#client{buffer=Rest}};
        {more, State} ->
            case recv(Client) of
                {ok, Data} -> next_chunk(stamp_recv(Client#client{buffer=Data}), State);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Stream chunked content, one chunk at a time. Does not decode. Supports
%% receiving continuations for repeated usage in a loop if a chunk is too large
%% to fit a single decoded packet. In this case, the partial content is returned
%% (so it can be streamed) and the caller is told more data is required, in
%% which case just calling again with the continuation will eventually do it.
-spec stream_chunk(client() | Continuation) -> LastChunk | Chunk | PartialChunk | {error, term()} when
        LastChunk :: {done, iodata(), client()},
        Chunk :: {ok, iodata(), client()},
        PartialChunk :: {more, pos_integer(), iodata(), Continuation},
        Continuation ::  {client(), term()}.
stream_chunk({Client, Cont}) -> stream_chunk(Client, stream_chunk, Cont);
stream_chunk(Client=#client{}) -> stream_chunk(Client, stream_chunk, undefined).

%% @doc Stream chunked content, one chunk at a time. Does decoding. Supports
%% receiving continuations for repeated usage in a loop if a chunk is too large
%% to fit a single decoded packet. In this case, the partial content is returned
%% (so it can be streamed) and the caller is told more data is required, in
%% which case just calling again with the continuation will eventually do it.
-spec stream_unchunk(client() | Continuation) -> LastChunk | Chunk | PartialChunk | {error, term()} when
        LastChunk :: {done, iodata(), client()},
        Chunk :: {ok, iodata(), client()},
        PartialChunk :: {more, pos_integer(), iodata(), Continuation},
        Continuation ::  {client(), term()}.
stream_unchunk({Client, Cont}) -> stream_chunk(Client, stream_unchunk, Cont);
stream_unchunk(Client) -> stream_chunk(Client, stream_unchunk, undefined).

%% @private
stream_chunk(Client=#client{buffer=Buffer}, StreamFun, Cont) ->
    case iolist_size(Buffer) of
        0 ->
            case recv(Client) of
                {ok, Data} ->
                    stream_chunk(stamp_recv(Client#client{buffer=Data}),
                                 StreamFun,
                                 Cont);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            case vegur_chunked:StreamFun(Buffer, Cont) of
                {done, Buf, Rest} ->
                    {done, Buf, set_stats(Client#client{buffer=Rest,
                                                        response_body=undefined})};
                {maybe_done, Buf, State} ->
                    %% Wait a short amount of time, worst case we rush it through.
                    case recv(Client#client{read_timeout=0}) of
                        {ok, Data} ->
                            {more, 0, Buf, {stamp_recv(Client#client{buffer=Data}),
                                            State}};
                        {error, timeout} ->
                            {done, Buf, set_stats(Client#client{buffer = <<>>,
                                                                response_body=undefined})};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {chunk, Buf, Rest} ->
                    {ok, Buf, Client#client{buffer=Rest}};
                {more, Len, Buf, State} ->
                    {more, Len, Buf, {Client#client{buffer = <<>>}, State}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @private
%% Stream the body until the socket is closed.
response_body_close(Client, Acc) ->
    case stream_close(Client) of
        {done, Client2} ->
            {ok, Acc, set_stats(Client2)};
        {ok, Body, Client2} ->
            response_body_close(Client2, [Acc, Body]);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Stream data when the response body is defined to be close-delimited.
%% In such cases, the total body size is unknown, and only the closing of
%% the connection by the responder determines when the data transfer has
%% been completed.
-spec stream_close(client()) -> {ok, iodata(), client()}
                              | {done, client()}
                              | {error, term()}.
stream_close(Client=#client{bytes_sent=undefined, bytes_recv=undefined}) ->
    % Since this stream will end on a socket close, I need to dump the
    % statistics currently associated with the socket before streaming and
    % keeping the books manually
    stream_close(set_stats(Client));
stream_close(Client=#client{buffer=Buffer, response_body=undefined, bytes_recv=BytesRecv}) ->
    case byte_size(Buffer) of
        0 ->
            case recv(Client) of
                {ok, Body} ->
                    {ok, Body, stamp_recv(Client#client{
                        bytes_recv=BytesRecv+iolist_size(Body)
                     })};
                {error, closed} ->
                    {done, Client};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {ok, Buffer, Client#client{buffer = <<>>}}
    end.

%% @doc Stream the content body of a response (no matter its size) to
%% nowhere as we want it ignored. Allows to clean up a connection where
%% bodies are unwanted for keep-alive purposes. If the body is to be ignored
%% and the connection not reused, it is cheaper to just close the socket,
%% as we avoid copying data to memory and requiring to GC it.
-spec skip_body(client()) -> {done, client()} | {error, term()}.
skip_body(Client=#client{state=response_body}) ->
    case stream_body(Client) of
        {ok, _, Client2} -> skip_body(Client2);
        Done -> Done
    end.

%% @doc Returns the status of the request along with the description it
%% came with.
-spec stream_status(client()) ->
    {ok, Status, StatusStr, client()} | {error, term()} when
      Status :: 100..999,
      StatusStr :: binary().
stream_status(Client=#client{state=State, buffer=Buffer})
        when State =:= request ->
    case binary:split(Buffer, <<"\r\n">>) of
        [Line, Rest] ->
            parse_version(Client#client{state=response, buffer=Rest}, Line);
        [<<>>] -> % first call
                    case recv(Client) of
                        {ok, Data} ->
                            Buffer2 = << Buffer/binary, Data/binary >>,
                            stream_status(stamp_recv(Client#client{buffer=Buffer2}));
                        {error, Reason} ->
                            {error, Reason}
                    end;
        _ ->
            MaxStatus = vegur_utils:config(max_client_status_length),
            case byte_size(Buffer) > MaxStatus of
                true ->
                    {error, status_length};
                false ->
                    case recv(Client) of
                        {ok, Data} ->
                            Buffer2 = << Buffer/binary, Data/binary >>,
                            stream_status(stamp_recv(Client#client{buffer=Buffer2}));
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @private
parse_version(Client, << "HTTP/1.1 ", Rest/binary >>) ->
    parse_status(Client, Rest, 'HTTP/1.1');
parse_version(Client, << "HTTP/1.0 ", Rest/binary >>) ->
    parse_status(Client, Rest, 'HTTP/1.0');
parse_version(_, _) ->
    {error, invalid_status}.

%% @private
parse_status(Client, << S3, S2, S1 >>, Version)
  when S3 >= $0, S3 =< $9, S2 >= $0, S2 =< $9, S1 >= $0, S1 =< $9 ->
    Status = (S3 - $0) * 100 + (S2 - $0) * 10 + S1 - $0,
    {ok, Status, ?REASON_MISSING, Client#client{version=Version,
                                                status=Status}};
parse_status(Client, << S3, S2, S1, " ", StatusStr/binary >>, Version)
        when S3 >= $0, S3 =< $9, S2 >= $0, S2 =< $9, S1 >= $0, S1 =< $9 ->
    Status = (S3 - $0) * 100 + (S2 - $0) * 10 + S1 - $0,
    {ok, Status, StatusStr, Client#client{version=Version,
                                          status=Status}};
parse_status(_Client, _StatusStr, _Version_) ->
    {error, invalid_status}.

%% @doc Returns headers from the response one at a time, until done.
-spec stream_headers(client()) -> {ok, binary(), binary(), client()}
                                | {done, client()}
                                | {error, term()}.
stream_headers(Client=#client{state=State})
        when State =:= response ->
    stream_headers(Client, []).

%% @private
stream_headers(Client, Acc) ->
    MaxLine = vegur_utils:config(max_client_header_length),
    stream_headers(Client, Acc, MaxLine).

%% @private
stream_headers(Client, Acc, MaxLine) ->
    case stream_header(Client, MaxLine) of
        {ok, Name, Value, Client2} ->
            stream_headers(Client2, [{Name, Value}|Acc], MaxLine);
        {done, Client2} ->
            {ok, lists:reverse(Acc), Client2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Returns headers from the response one at a time, until done.
%% Favor the use of `stream_headers/1' to this function, as it performs
%% state checks about the request, unless you know why you'd want to break
%% this (i.e. using a raw socket and wanting it parsed by this).
-spec stream_header(client()) -> {ok, binary(), binary(), client()}
                               | {done, client()}
                               | {error, term()}.
stream_header(Client) ->
    MaxLine = vegur_utils:config(max_client_header_length),
    stream_header(Client, MaxLine).

%% @private
stream_header(Client=#client{state=State, buffer=Buffer,
        response_body=RespBody}, MaxLine) when State =:= response ->
    case binary:split(Buffer, <<"\r\n">>) of
        [<<>>, Rest] ->
            %% If we have a body, set response_body.
            Client2 = case RespBody of
                undefined -> Client#client{state=request};
                0 -> Client#client{state=request};
                _ -> Client#client{state=response_body}
            end,
            {done, Client2#client{buffer=Rest}};
        [Line, Rest] ->
            %% @todo Do a better parsing later on.
            [Name, Value] = binary:split(Line, [<<": ">>, <<":">>]),
            Name2 = cowboyku_bstr:to_lower(Name),
            MaybeClient = case Name2 of
                <<"content-length">> ->
                    try
                        Length = list_to_integer(binary_to_list(Value)),
                        case RespBody of
                            Length -> % duplicate, absorb
                                skip;
                            chunked -> % chunked has priority
                                Client;
                            undefined when Length >= 0 -> % first time seen
                                Client#client{response_body=Length};
                            _ -> % length not matching
                                {error, content_length}
                        end
                    catch
                        error:badarg -> {error, content_length};
                        error:case_clause -> {error, content_length}
                    end;
                <<"transfer-encoding">> ->
                    Values = cowboyku_http:nonempty_list(Value, fun cowboyku_http:token_ci/2),
                    case Values of
                        {error, badarg} ->
                            invalid_transfer_encoding(Value),
                            {error, invalid_transfer_encoding};
                        Values ->
                            case lists:member(<<"chunked">>, Values) of
                                true -> Client#client{response_body=chunked};
                                false -> Client
                            end
                    end;
                <<"connection">> ->
                    %% Allow empty list
                    Values = cowboyku_http:list(Value, fun cowboyku_http:token_ci/2),
                    case lists:member(<<"close">>, Values) of
                        true -> Client#client{connection=close};
                        false ->
                            case lists:member(<<"keepalive">>, Values) of
                                true -> Client#client{connection=keepalive};
                                false -> Client
                            end
                    end;
                <<"set-cookie">> ->
                    MaxCookie = vegur_utils:config(max_client_cookie_length),
                    case byte_size(Line) > MaxCookie of
                        true -> {error, cookie_length};
                        false -> Client
                    end;
                _ ->
                    Client
            end,
            case MaybeClient of
                skip ->
                    stream_header(Client#client{buffer=Rest}, MaxLine);
                Client2=#client{} ->
                    {ok, Name2, Value, Client2#client{buffer=Rest}};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            case iolist_size(Buffer) > MaxLine of
                true ->
                    {error, header_length};
                false ->
                    case recv(Client) of
                        {ok, Data} ->
                            Buffer2 = << Buffer/binary, Data/binary >>,
                            stream_header(stamp_recv(Client#client{buffer=Buffer2}), MaxLine);
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

%% @doc Stream out a regular (unchunked, not close-delimited) body
-spec stream_body(client()) -> {ok, iodata(), client()}
                             | {done, client()}
                             | {error, term()}.
stream_body(Client=#client{state=response_body, response_body=RespBody})
        when RespBody =:= undefined; RespBody =:= 0 ->
    {done, Client#client{state=request, response_body=undefined}};
stream_body(Client=#client{state=response_body, buffer=Buffer,
        response_body=Length}) when is_integer(Length) ->
    case byte_size(Buffer) of
        0 ->
            case recv(Client) of
                {ok, Body} when byte_size(Body) =< Length ->
                    Length2 = Length - byte_size(Body),
                    {ok, Body, stamp_recv(Client#client{response_body=Length2})};
                {ok, Data} ->
                    << Body:Length/binary, Rest/binary >> = Data,
                    {ok, Body, stamp_recv(Client#client{buffer=Rest,
                        response_body=undefined
                     })};
                {error, Reason} ->
                    {error, Reason}
            end;
        N when N =< Length ->
            Length2 = Length - N,
            {ok, Buffer, Client#client{buffer= <<>>, response_body=Length2}};
        _ ->
            << Body:Length/binary, Rest/binary >> = Buffer,
            {ok, Body, Client#client{buffer=Rest, response_body=undefined}}
    end.

%% @private

%% TODO: This function exists to do some production tracing to get a
%% sampling of invalid transfer encodings we might be seeing in
%% production. Can safely be removed when the investigation is over.
invalid_transfer_encoding(X)->
    X.

recv(#client{socket=Socket, transport=Transport, first_read_timeout=undefined, read_timeout=Timeout}) ->
    Transport:recv(Socket, 0, Timeout);
recv(#client{socket=Socket, transport=Transport, first_read_timeout=Timeout}) ->
    Transport:recv(Socket, 0, Timeout).

%% @doc
%% Asks vegur_client to fetch data from the socket, and store it in its
%% internal buffer, up to a certain size. If the buffer is full, this
%% function does nothing.
%% A length of 0 means we want any amount of data buffered, including what
%% is already there. No limits!
-spec buffer_data(0 | pos_integer(), timeout(), client()) -> {ok, client()} | {error, term()}.
buffer_data(0, Timeout, Client=#client{socket=Socket, transport=Transport,
                                       buffer= <<>>}) ->
    case Transport:recv(Socket, 0, Timeout) of
        {ok, Data} ->
            {ok, stamp_recv(Client#client{buffer=Data})};
        {error, Reason} ->
            {error, Reason}
    end;
buffer_data(0, _Timeout, Client) -> % data is in the buffer
    {ok, Client};
buffer_data(Length, Timeout, Client=#client{socket=Socket, transport=Transport,
                                            buffer=Buffer}) ->
    case Length - iolist_size(Buffer) of
        N when N > 0 ->
            case Transport:recv(Socket, N, Timeout) of
                {ok, Data} ->
                    {ok, stamp_recv(Client#client{
                        buffer= <<Buffer/binary, Data/binary>>
                     })};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {ok, Client}
    end.

%% @doc Append arbitrary data to vegur_client's internal buffer.
%% Can be useful with borrowed sockets. Use at your own risk.
-spec append_to_buffer(binary(), client()) -> client().
append_to_buffer(Data, Client=#client{buffer = Buffer}) ->
    Client#client{buffer = <<Buffer/binary, Data/binary>>}.

%% @private

%% @doc Generates an authorization header from a string of the form
%% `"User:Pass"' or `"User"'.
-spec auth_header(string()) -> [{binary(), iodata()}].
auth_header("") ->
    [];
auth_header(AuthInfo) when is_list(AuthInfo) ->
    [{<<"Authorization">>,
      case string:tokens(AuthInfo, ":") of
          [User, Pass] ->
              encode_auth_header(User, Pass);
          [User] ->
              encode_auth_header(User)
      end}].

%% @private
encode_auth_header(User) ->
    encode_auth_header(User, "").

%% @private
encode_auth_header(User, Pass)
  when is_list(User), is_list(Pass) ->
    ["Basic ", base64:encode(User ++ ":" ++ Pass)].

%% @doc Fetch statistics about bytes shuttled in and out of the client.
%% Stats obtained from the inets functionality. The values may be `undefined'
%% when the call is made and no connection has been established.
-spec byte_counts(client()) -> {BytesSent, BytesRecv} when
      BytesSent :: non_neg_integer() | undefined,
      BytesRecv :: non_neg_integer() | undefined.
byte_counts(Client) ->
    #client{bytes_sent=BytesSent, bytes_sent_offset=BytesSentOffset,
            bytes_recv=BytesRecv, bytes_recv_offset=BytesRecvOffset} = set_stats(Client),
    case {BytesSent, BytesRecv} of
        {undefined, undefined} -> {undefined, undefined};
        {undefined, _} -> {undefined, BytesRecv+BytesRecvOffset};
        {_, undefined} -> {BytesSent + BytesSentOffset};
        {_, _} -> {BytesSent+BytesSentOffset, BytesRecv+BytesRecvOffset}
    end.

%% @doc Updates statistics about bytes shuttled in and out of the client.
%% Stats are obtained from the inets functionality on sockets and is
%% snapshotted within the client's state.
-spec set_stats(client()) -> client().
set_stats(Client=#client{socket=undefined}) ->
    Client;
set_stats(Client=#client{bytes_sent=BytesSent, bytes_recv=BytesRecv,
                         socket=Socket}) ->
    {Sent, Recv} = get_stats(Socket, BytesSent, BytesRecv),
    Client#client{bytes_sent=Sent, bytes_recv=Recv}.

%% @private Takes the current value of data sent and received in bytes
%% and stores it as an offset. This allows individual requests over
%% keepalive connections to maintain independent statistics despite
%% the ever-incrementing byte counts returned by the socket.
-spec set_delta(client()) -> client().
set_delta(Client=#client{socket=undefined}) ->
    Client;
set_delta(Client=#client{bytes_sent=BytesSent, bytes_recv=BytesRecv}) ->
    Client#client{bytes_sent_offset= -BytesSent, bytes_recv_offset= -BytesRecv}.

%% @private
get_stats(Socket, DefaultSent, DefaultRecv) when is_port(Socket) ->
    case inet:getstat(Socket, [recv_oct, send_oct]) of
        {error, _} ->
            {DefaultSent, DefaultRecv};
        {ok, [{recv_oct, RecvTotal},
              {send_oct, SentTotal}]} ->
            {SentTotal, RecvTotal}
    end.

%% @doc Fetches the request log of a given client.
-spec log(client()) -> vegur_req_log:request_log().
log(#client{first_packet_recv=RecvFirst, last_packet_recv=RecvLast,
            first_packet_sent=SentFirst, last_packet_sent=SentLast,
            log=Log}) ->
    Log2 = lists:foldl(fun({undefined, _}, TmpLog) -> TmpLog;
                          ({Time,Name}, TmpLog) -> vegur_req_log:stamp(Name, Time, TmpLog)
                       end,
                       vegur_req_log:new(os:timestamp()),
                       lists:sort([{RecvFirst, client_first_packet_recv},
                                   {RecvLast, client_last_packet_recv},
                                   {SentFirst, client_first_packet_sent},
                                   {SentLast, client_last_packet_sent}])),
    vegur_req_log:merge([Log, Log2]).

%% @private
reset_log(C=#client{log=_Log}) ->
    Now = os:timestamp(),
    NewLog = vegur_req_log:stamp(client_connect, Now, vegur_req_log:new(Now)),
    C#client{first_packet_recv=undefined, last_packet_recv=undefined,
             first_packet_sent=undefined, last_packet_sent=undefined,
             log = NewLog}.

%% @private
stamp_sent(Client=#client{first_packet_sent=First}) ->
    Now = os:timestamp(),
    case First of
        undefined -> Client#client{first_packet_sent=Now, last_packet_sent=Now};
        _ -> Client#client{last_packet_sent=Now}
    end.

%% @private
stamp_recv(Client=#client{first_packet_recv=First}) ->
    Now = os:timestamp(),
    case First of
        undefined ->
            Client#client{first_packet_recv=Now, last_packet_recv=Now,
                          first_read_timeout=undefined};
        _ ->
            Client#client{last_packet_recv=Now}
    end.
