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


-record(client, {
          state = wait :: wait | request | response | response_body | raw,
          opts = [] :: [any()],
          socket = undefined :: undefined | inet:socket(),
          transport = undefined :: module(),
          connect_timeout = 3100 :: timeout(), %% @todo Configurable.
          read_timeout = 5000 :: timeout(), %% @todo Configurable.
          buffer = <<>> :: binary(),
          connection = keepalive :: keepalive | close,
          version = 'HTTP/1.1' :: cowboy:http_version(),
          response_body = undefined :: chunked | undefined | non_neg_integer(),
          bytes_sent :: non_neg_integer() | undefined, %% Bytes sent downstream
          bytes_recv :: non_neg_integer() | undefined %% Bytes recv from downstream
}).

-opaque client() :: #client{}.
-export_type([client/0]).

init(Opts) ->
    {ok, #client{opts=Opts}}.

state(#client{state=State}) ->
    State.

transport(#client{socket=undefined}) ->
    {error, notconnected};
transport(#client{transport=Transport, socket=Socket}) ->
    {ok, Transport, Socket}.

close(#client{socket=undefined}=Client) ->
    Client;
close(#client{transport=Transport, socket=Socket}=Client) ->
    NewClient=set_stats(Client),
    Transport:close(Socket),
    NewClient#client{socket=undefined}.

connect(Transport, Host, Port, Timeout, Client) ->
    connect(Transport, Host, Port, Client#client{connect_timeout=Timeout}).

connect(Transport, Host, Port, Client)
        when is_binary(Host) ->
    connect(Transport, binary_to_list(Host), Port, Client);
connect(Transport, Host, Port,
        Client=#client{state=wait, opts=Opts, connect_timeout=Timeout})
        when is_atom(Transport),
            (is_list(Host) orelse is_tuple(Host)),
            is_integer(Port) ->
    case Transport:connect(Host, Port, Opts, Timeout) of
        {ok, Socket} ->
            {ok, Client#client{state=request,
                               socket=Socket,
                               transport=Transport}};
        {error, _} = Err -> Err
    end.

raw_request(Data, Client=#client{state=response_body}) ->
    {done, Client2} = skip_body(Client),
    raw_request(Data, Client2);
raw_request(Data, Client=#client{
        state=request, socket=Socket, transport=Transport}) ->
    case Transport:send(Socket, Data) of
        ok ->
            {ok, set_stats(Client)};
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
    ContentLength = case Body of
        {stream, 0} -> [];
        {stream, chunked} -> [];
        {stream, Length} -> [{<<"Content-Length">>, integer_to_list(Length)}];
        Body -> [{<<"Content-Length">>, integer_to_list(iolist_size(Body))}]
    end,
    HeadersData = headers_to_iolist(Headers2++ContentLength),
    [Method, <<" ">>, Path, <<" ">>, VersionBin, <<"\r\n">>,
     HeadersData, <<"\r\n">>].

headers_to_iolist(Headers) ->
    [[Name, <<": ">>, Value, <<"\r\n">>] || {Name, Value} <- Headers].

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
    {done, Client2} = skip_body(Client),
    response(set_stats(Client2));
response(Client=#client{state=request}) ->
    case stream_status(Client) of
        {ok, Status, _, Client2} ->
            case stream_headers(Client2) of
                {ok, Headers, Client3} ->
                    {ok, Status, Headers, set_stats(Client3)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

body_type(#client{response_body=chunked}) -> chunked;
body_type(#client{state=request, response_body=0}) -> no_body;
body_type(#client{state=response_body, response_body=Length}) -> {content_size, Length};
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
                {ok, Data} -> next_chunk(Client#client{buffer=Data}, State);
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

stream_chunk({Client, Cont}) -> stream_chunk(Client, vegur_chunked, Cont);
stream_chunk(Client) -> stream_chunk(Client, vegur_chunked, undefined).

stream_unchunk({Client, Cont}) -> stream_chunk(Client, vegur_unchunked, Cont);
stream_unchunk(Client) -> stream_chunk(Client, vegur_unchunked, undefined).

stream_chunk(Client=#client{buffer=Buffer}, ChunkMod, Cont) ->
    case iolist_size(Buffer) of
        0 ->
            case recv(Client) of
                {ok, Data} -> stream_chunk(Client#client{buffer=Data}, ChunkMod, Cont);
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            case ChunkMod:stream_chunk(Buffer, Cont) of
                {done, Buf, Rest} ->
                    {done, Buf, set_stats(Client#client{buffer=Rest,
                                                        response_body=undefined})};
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
                    {ok, Body, Client#client{bytes_recv=BytesRecv+iolist_size(Body)}};
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
                            stream_status(Client#client{buffer=Buffer2});
                        {error, Reason} ->
                            {error, Reason}
                    end;
        _ ->
            MaxStatus = vegur_app:config(max_client_status_length, 8192),
            case byte_size(Buffer) > MaxStatus of
                true ->
                    {error, status_length};
                false ->
                    case recv(Client) of
                        {ok, Data} ->
                            Buffer2 = << Buffer/binary, Data/binary >>,
                            stream_status(Client#client{buffer=Buffer2});
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

parse_status(Client, << S3, S2, S1, " ", StatusStr/binary >>, Version)
        when S3 >= $0, S3 =< $9, S2 >= $0, S2 =< $9, S1 >= $0, S1 =< $9 ->
    Status = (S3 - $0) * 100 + (S2 - $0) * 10 + S1 - $0,
    {ok, Status, StatusStr, Client#client{version=Version}};
parse_status(_Client, _StatusStr, _Version_) ->
    {error, invalid_status}.


stream_headers(Client=#client{state=State})
        when State =:= response ->
    stream_headers(Client, []).

stream_headers(Client, Acc) ->
    MaxLine = vegur_app:config(max_client_header_length, 524288), %512k
    stream_headers(Client, Acc, MaxLine).

stream_headers(Client, Acc, MaxLine) ->
    case stream_header(Client, MaxLine) of
        {ok, Name, Value, Client2} ->
            stream_headers(Client2, [{Name, Value}|Acc], MaxLine);
        {done, Client2} ->
            {ok, Acc, Client2};
        {error, Reason} ->
            {error, Reason}
    end.

stream_header(Client) ->
    MaxLine = vegur_app:config(max_client_header_length, 524288), %512k
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
                    MaxCookie = vegur_app:config(max_client_cookie_length, 8192),
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
                            stream_header(Client#client{buffer=Buffer2}, MaxLine);
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
                    {ok, Body, Client#client{response_body=Length2}};
                {ok, Data} ->
                    << Body:Length/binary, Rest/binary >> = Data,
                    {ok, Body, Client#client{buffer=Rest,
                        response_body=undefined}};
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
            {ok, Client#client{buffer=Data}};
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
                    {ok, Client#client{buffer= <<Buffer/binary, Data/binary>>}};
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
