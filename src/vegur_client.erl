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
%% Copyright (c) 2013, Heroku <nem@erlang.geek.nz>
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

-export([body_type/1]).
-export([version/1]).

-export([headers_to_iolist/1]).
-export([request_to_headers_iolist/6]).
-export([request_to_iolist/6]).
-export([raw_socket/1]).
-export([auth_header/1]).

-export([set_stats/1]).
-export([byte_counts/1]).
-export([log/1]).

-define(REASON_MISSING, <<"">>). %% "EBADDEVELOPER"

-record(client, {
          state = wait :: wait | request | response | response_body | raw,
          opts = [] :: [any()],
          socket = undefined :: undefined | inet:socket(),
          transport = undefined :: module() | tuple(), % tuple for tuple calls
          connect_timeout = vegur_utils:config(downstream_connect_timeout) :: timeout(),
          read_timeout = timer:seconds(vegur_utils:config(downstream_timeout)) :: timeout(),
          buffer = <<>> :: binary(),
          connection = keepalive :: keepalive | close,
          version = 'HTTP/1.1' :: cowboy:http_version(),
          status = undefined :: 100..999,
          response_body = undefined :: chunked | undefined | non_neg_integer(),
          bytes_sent :: non_neg_integer() | undefined, % Bytes sent downstream
          bytes_recv :: non_neg_integer() | undefined, % Bytes recv from downstream
          first_packet_recv :: undefined | erlang:timestamp(),
          last_packet_recv :: undefined | erlang:timestamp(),
          first_packet_sent :: undefined | erlang:timestamp(),
          last_packet_sent :: undefined | erlang:timestamp(),
          log :: vegur_req_log:request_log(),
          expect_trailers = false :: boolean()
}).

-type client() :: #client{}.
-export_type([client/0]).

-spec init([any()]) -> {ok, client()}.
init(Opts) ->
    {ok, #client{opts=Opts, log=vegur_req_log:new(os:timestamp())}}.

state(#client{state=State}) ->
    State.

transport(#client{socket=undefined}) ->
    {error, notconnected};
transport(#client{transport=Transport, socket=Socket}) ->
    {ok, Transport, Socket}.

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

connect(Transport, Host, Port, Timeout, Client) ->
    connect(Transport, Host, Port, Client#client{connect_timeout=Timeout}).

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

request(Method, URL, Client) ->
    request(Method, URL, [], <<>>, Client).

request(Method, URL, Headers, Client) ->
    request(Method, URL, Headers, <<>>, Client).

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

request_to_headers_iolist(Method, Headers, Body, Version, FullHost, Path) ->
    VersionBin = atom_to_binary(Version, latin1),
    %% @todo do keepalive too, allow override...
    Headers2 = [{<<"Host">>, FullHost} | Headers],
    ContentLength = content_length_header(Method, Body),
    HeadersData = headers_to_iolist(Headers2++ContentLength),
    [Method, <<" ">>, Path, <<" ">>, VersionBin, <<"\r\n">>,
     HeadersData, <<"\r\n">>].

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

headers_to_iolist(Headers) ->
    [[cowboy_bstr:capitalize_token(Name), <<": ">>, Value, <<"\r\n">>] || {Name, Value} <- Headers].

request_to_iolist(Method, Headers, Body, Version, FullHost, Path) ->
    [request_to_headers_iolist(Method, Headers, Body, Version, FullHost, Path),
     Body].


raw_socket(Client=#client{transport=T, socket=S, buffer=Buf}) ->
    {{T,S}, Buf, Client#client{buffer= <<>>, state=raw}}.

parse_url(<< "https://", Rest/binary >>) ->
    parse_url(Rest, ranch_ssl);
parse_url(<< "http://", Rest/binary >>) ->
    parse_url(Rest, ranch_tcp);
parse_url(URL) ->
    parse_url(URL, ranch_tcp).

parse_url(URL, Transport) ->
    case binary:split(URL, <<"/">>) of
        [Peer] ->
            {Host, Port} = parse_peer(Peer, Transport),
            {Transport, Peer, Host, Port, <<"/">>};
        [Peer, Path] ->
            {Host, Port} = parse_peer(Peer, Transport),
            {Transport, Peer, Host, Port, [<<"/">>, Path]}
    end.

parse_peer(Peer, Transport) ->
    case binary:split(Peer, <<":">>) of
        [Host] when Transport =:= ranch_tcp ->
            {binary_to_list(Host), 80};
        [Host] when Transport =:= ranch_ssl ->
            {binary_to_list(Host), 443};
        [Host, Port] ->
            {binary_to_list(Host), list_to_integer(binary_to_list(Port))}
    end.

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

body_type(#client{response_body=chunked}) -> chunked;
body_type(#client{state=request, response_body=0}) -> no_body;
body_type(#client{state=response_body, response_body=Length}) -> {content_size, Length};
body_type(#client{state=request, status=204, response_body=undefined}) -> no_body;
body_type(#client{state=request, status=304, response_body=undefined}) -> no_body;
body_type(#client{state=request}) -> stream_close.

version(#client{version=Version}) -> Version.

response_body(Client=#client{response_body=chunked}) ->
    response_body_chunk(Client, <<>>);
response_body(Client=#client{state=response_body}) ->
    response_body_loop(Client, <<>>);
response_body(Client=#client{state=request, connection=close}) ->
    response_body_close(Client, <<>>).

response_body_loop(Client, Acc) ->
    case stream_body(Client) of
        {ok, Data, Client2} ->
            response_body_loop(Client2, << Acc/binary, Data/binary >>);
        {done, Client2} ->
            {ok, Acc, set_stats(Client2)};
        {error, Reason} ->
            {error, Reason}
    end.

%% WARNING: we only support chunked reading where we get back
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
        {maybe_done, State} ->
            case recv(Client#client{read_timeout=0}) of
                {ok, Data} ->
                    next_chunk(Client#client{buffer=Data}, State);
                {error, timeout} ->
                    next_chunk(Client#client{buffer= <<>>}, State);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

stream_chunk({Client, Cont}) -> stream_chunk(Client, stream_chunk, Cont);
stream_chunk(Client=#client{expect_trailers=Trailers}) ->
    %% Detect if we wait for trailers. Only do this for chunked transfers,
    %% never for 'unchunked' ones as this doesn't carry over.
    if Trailers ->
            stream_chunk(Client, stream_chunk, trailers)
     ; not Trailers ->
            stream_chunk(Client, stream_chunk, undefined)
    end.

stream_unchunk({Client, Cont}) -> stream_chunk(Client, stream_unchunk, Cont);
stream_unchunk(Client) -> stream_chunk(Client, stream_unchunk, undefined).

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

skip_body(Client=#client{state=response_body}) ->
    case stream_body(Client) of
        {ok, _, Client2} -> skip_body(Client2);
        Done -> Done
    end.

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

parse_version(Client, << "HTTP/1.1 ", Rest/binary >>) ->
    parse_status(Client, Rest, 'HTTP/1.1');
parse_version(Client, << "HTTP/1.0 ", Rest/binary >>) ->
    parse_status(Client, Rest, 'HTTP/1.0');
parse_version(_, _) ->
    {error, invalid_status}.

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


stream_headers(Client=#client{state=State})
        when State =:= response ->
    stream_headers(Client, []).

stream_headers(Client, Acc) ->
    MaxLine = vegur_utils:config(max_client_header_length),
    stream_headers(Client, Acc, MaxLine).

stream_headers(Client, Acc, MaxLine) ->
    case stream_header(Client, MaxLine) of
        {ok, Name, Value, Client2} ->
            stream_headers(Client2, [{Name, Value}|Acc], MaxLine);
        {done, Client2} ->
            {ok, lists:reverse(Acc), Client2};
        {error, Reason} ->
            {error, Reason}
    end.

stream_header(Client) ->
    MaxLine = vegur_utils:config(max_client_header_length),
    stream_header(Client, MaxLine).

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
            Name2 = cowboy_bstr:to_lower(Name),
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
                    case lists:member(<<"chunked">>, header_list_values(Value)) of
                        true -> Client#client{response_body=chunked};
                        false -> Client
                    end;
                <<"connection">> ->
                    Values = header_list_values(Value),
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
                <<"trailer">> ->
                    Client#client{expect_trailers=true};
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

recv(#client{socket=Socket, transport=Transport, read_timeout=Timeout}) ->
    Transport:recv(Socket, 0, Timeout).

%% A length of 0 means we want any amount of data buffered, including what
%% is already there
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

header_list_values(Value) ->
    cowboy_http:nonempty_list(Value, fun cowboy_http:token_ci/2).

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

byte_counts(Client) ->
    #client{bytes_sent=BytesSent, bytes_recv=BytesRecv} = set_stats(Client),
    {BytesSent, BytesRecv}.

set_stats(Client=#client{socket=undefined}) ->
    Client;
set_stats(Client=#client{bytes_sent=BytesSent, bytes_recv=BytesRecv,
                         socket=Socket}) ->
    {Sent, Recv} = get_stats(Socket, BytesSent, BytesRecv),
    Client#client{bytes_sent=Sent, bytes_recv=Recv}.

get_stats(Socket, DefaultSent, DefaultRecv) when is_port(Socket) ->
    case inet:getstat(Socket, [recv_oct, send_oct]) of
        {error, _} ->
            {DefaultSent, DefaultRecv};
        {ok, [{recv_oct, RecvTotal},
              {send_oct, SentTotal}]} ->
            {SentTotal, RecvTotal}
    end.

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

stamp_sent(Client=#client{first_packet_sent=First}) ->
    Now = os:timestamp(),
    case First of
        undefined -> Client#client{first_packet_sent=Now, last_packet_sent=Now};
        _ -> Client#client{last_packet_sent=Now}
    end.

stamp_recv(Client=#client{first_packet_recv=First}) ->
    Now = os:timestamp(),
    case First of
        undefined -> Client#client{first_packet_recv=Now, last_packet_recv=Now};
        _ -> Client#client{last_packet_recv=Now}
    end.
