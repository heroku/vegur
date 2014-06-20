%%% Chunked encoding delimitation according to
%%% http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.6.1
%%       Chunked-Body   = *chunk
%%                        last-chunk
%%                        trailer
%%                        CRLF
%%
%%       chunk          = chunk-size [ chunk-extension ] CRLF
%%                        chunk-data CRLF
%%       chunk-size     = 1*HEX
%%       last-chunk     = 1*("0") [ chunk-extension ] CRLF
%%
%%       chunk-extension= *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
%%       chunk-ext-name = token
%%       chunk-ext-val  = token | quoted-string
%%       chunk-data     = chunk-size(OCTET)
%%       trailer        = *(entity-header CRLF)
%%
%%% For brevity in parsing, we consider 'token' to be any character not to
%%% be a delimiter, whereas the RFC defines them as
%%% '1*<any CHAR except CTLs or separators>', where CTL is defined as
%%% (octets 0 - 31) and DEL (127).
%%%
%%% We also do not support extensions and will discard them:
%%
%% All HTTP/1.1 applications MUST be able to receive and decode the "chunked"
%% transfer-coding, and MUST ignore chunk-extension extensions they do not
%% understand.
-module(vegur_chunked).
-export([next_chunk/1, next_chunk/2,
         next_unchunk/1, next_unchunk/2,
         stream_chunk/1, stream_chunk/2,
         stream_unchunk/1, stream_unchunk/2,
         all_chunks/1, all_unchunks/1]).
-record(state, {buffer = <<>> :: binary(),
                length :: non_neg_integer()|undefined,
                trailers = undefined,
                type = chunked :: chunked|unchunked}).

%% Parses a binary stream to get the next chunk in it.
next_unchunk(Bin) -> next_unchunk(Bin, undefined).

next_unchunk(Bin, undefined) ->
    chunk_size(Bin, #state{type=unchunked});
next_unchunk(Bin, {_, #state{}}=S) -> next_chunk(Bin, S).

next_chunk(Bin) -> next_chunk(Bin, undefined).

next_chunk(Bin, undefined) ->
    chunk_size(Bin, #state{type=chunked});
next_chunk(Bin, {Fun,State=#state{}}) -> Fun(Bin, State).

%% Parses a binary stream to get chunk delimitation. The difference from
%% next_chunk/1-2 is that this will not accumulate data, but simply return it
%% at each iteration, allowing to return partial chunks and a continuation
%% to get more later on without interrupting the parsing.
%%
%% This allows to know where the end of a message is in a stream, without
%% keeping any data in memory longer than necessary.
stream_unchunk(Bin) -> stream_unchunk(Bin, undefined).

stream_unchunk(Bin, undefined) -> stream_chunk(Bin, {fun chunk_size/2,
                                                     #state{type=unchunked}});
stream_unchunk(Bin, {_, #state{}}=S) -> stream_chunk(Bin, S).

stream_chunk(Bin) -> stream_chunk(Bin, undefined).

stream_chunk(Bin, undefined) -> stream_chunk(Bin, {fun chunk_size/2,
                                                   #state{type=chunked}});
stream_chunk(Bin, {Fun, State=#state{}}) ->
    case Fun(Bin, State) of
        {done, Buf, Rest} -> {done, Buf, Rest};
        {error, Reason} -> {error, Reason};
        {chunk, Buf, Rest} -> {chunk, Buf, Rest};
        {more, {NewFun, S=#state{buffer=Buf, length=Len}}} ->
            {more, Len, Buf, {NewFun,S#state{buffer = <<>>}}}
    end.

%% Fetch all the chunks in a binary stream, and error out if they're not
%% all there.
all_unchunks(Bin) -> all_chunks(Bin, fun next_unchunk/1, []).

all_chunks(Bin) -> all_chunks(Bin, fun next_chunk/1, []).

all_chunks(Bin, F, Acc) ->
    try F(Bin) of
        {done, Buf, Rest} -> {done, [Acc, Buf], Rest};
        {error, Reason} -> {error, Acc, Reason};
        {more, _State} -> ct:pal("State:~p",[_State]), {error, Acc, incomplete};
        {chunk, Buf, Rest} -> all_chunks(Rest, F, [Acc, Buf])
    catch
        error:function_clause -> {error, Acc, {bad_chunk, Bin}}
    end.

%% Trailers use not supported yet.
% next_chunk(Bin, Trailers) -> chunk_size(Bin, #state{trailers=Trailers}).

%%           ,-----------------v
%% chunk_size -> extension -> data
%%                |    ^
%%                V    |
%%          ext_name -> ext_val
%%                        | |
%%                    quoted string
%%

chunk_size(<<"">>, S=#state{}) ->
    {more, {fun chunk_size/2, S}};
chunk_size(<<"\r", Rest/binary>>, S=#state{}) ->
    chunk_size_cr(Rest, maybe_buffer(<<"\r">>, S));
chunk_size(<<";", Rest/binary>>, S=#state{length=Len}) ->
    case Len of
        undefined -> {error, {bad_chunk, no_length}};
        _ -> ext_name(Rest, S)
    end;
chunk_size(<<X, Rest/binary>>, S=#state{length=RawLen}) ->
    Len = case RawLen of
        undefined -> 0;
        RawLen -> RawLen
    end,
    if  X >= $0, X =< $9 ->
            N = X - $0,
            chunk_size(Rest, maybe_buffer(<<X>>, S#state{length=Len*16 + N}));
        X >= $A, X =< $F ->
            N = X - $A + 10,
            chunk_size(Rest, maybe_buffer(<<X>>, S#state{length=Len*16 + N}));
        X >= $a, X =< $f ->
            N = X - $a + 10,
            chunk_size(Rest, maybe_buffer(<<X>>, S#state{length=Len*16 + N}));
        true ->
            {error, {bad_chunk, {length_char, <<X>>}}}
    end.

chunk_size_cr(_Bin, #state{length=undefined}) ->
    {error, {bad_chunk, no_length}};
chunk_size_cr(<<"">>, S=#state{}) ->
    {more, {fun chunk_size_cr/2, S}};
chunk_size_cr(<<"\n", Rest/binary>>, S=#state{length=0}) ->
    last_chunk(Rest, maybe_buffer(<<"\n">>, S));
chunk_size_cr(<<"\n", Rest/binary>>, S=#state{}) ->
    chunk_data(Rest, maybe_buffer(<<"\n">>, S));
chunk_size_cr(<<Char, _/binary>>, #state{}) ->
    {error, {bad_chunk, {length_char, <<Char>>}}}.

%% extension handling is lax -- for example, the extension ';;;;=a'
%% is invalid as per the RFC, but we don't care, because we drop them anyway
ext_name(<<"">>, S=#state{}) ->
    {more, {fun ext_name/2, S}};
ext_name(<<"=", Rest/binary>>, S=#state{}) ->
    ext_val(Rest, S);
ext_name(<<"\r", Rest/binary>>, S=#state{}) ->
    chunk_size_cr(Rest, maybe_buffer(<<"\r">>, S));
ext_name(<<_, Rest/binary>>, S=#state{}) ->
    ext_name(Rest, S).

ext_val(<<"">>, S=#state{}) ->
    {more, {fun ext_val/2, S}};
ext_val(<<";", Rest/binary>>, S=#state{}) ->
    ext_name(Rest, S);
ext_val(<<"\r", Rest/binary>>, S=#state{}) ->
    chunk_size_cr(Rest, maybe_buffer(<<"\r">>, S));
ext_val(<<"\"", Rest/binary>>, S=#state{}) ->
    ext_quoted_string(Rest, S);
ext_val(<<_, Rest/binary>>, S=#state{}) ->
    ext_val(Rest, S).

ext_quoted_string(<<>>, S=#state{}) ->
    {more, {fun ext_quoted_string/2, S}};
ext_quoted_string(<<"\\\"", Rest/binary>>, S=#state{}) ->
    ext_quoted_string(Rest, S);
ext_quoted_string(<<"\"", Rest/binary>>, S=#state{}) ->
    ext_val(Rest, S);
ext_quoted_string(<<_, Rest/binary>>, S=#state{}) ->
    ext_quoted_string(Rest, S).

chunk_data(<<>>, S=#state{}) ->
    {more, {fun chunk_data/2, S}};
chunk_data(<<"\r", Rest/binary>>, S=#state{length=0}) ->
    chunk_data_cr(Rest, maybe_buffer(<<"\r">>, S));
chunk_data(<<Byte, Rest/binary>>, S=#state{length=N, buffer=Buf}) when N > 0 ->
    chunk_data(Rest, S#state{length=N-1, buffer = <<Buf/binary, Byte>>});
chunk_data(Bin, #state{buffer=Buf, length=0}) ->
    {error, {bad_chunk, {0, [Buf,Bin]}}}.

chunk_data_cr(<<>>, S=#state{}) ->
    {more, {fun chunk_data_cr/2, S}};
chunk_data_cr(<<"\n", Rest/binary>>, S=#state{length=0}) ->
    #state{buffer=Buf} = maybe_buffer(<<"\n">>, S),
    {chunk, Buf, Rest};
chunk_data_cr(Bin, #state{buffer=Buf, length=Len}) ->
    {error, {bad_chunk, {Len, [Buf,Bin]}}}.

last_chunk(<<>>, #state{buffer=Buf}) ->
    %% Consider this done, due to possibly having only a bad CRLF
    %% after the length, but not after the final chunk.
    %{more, {fun last_chunk/2, S}};
    {done, Buf, <<>>};
last_chunk(<<"\r", Rest/binary>>, S=#state{length=0}) ->
    last_chunk_cr(Rest, maybe_buffer(<<"\r">>, S));
last_chunk(Bin, #state{length=0, buffer=Buf}) ->
    %% Here we are lenient on a last chunk to allow all kinds of
    %% dumb stuff to happen. Uncommenting the line makes for a
    %% strict parser. This one here tolerates 0-length chunks halfway
    %% through a stream and whatnot.
    %{error, {bad_chunk, {0, [Buf,Bin]}}}.
    {done, Buf, Bin}.

last_chunk_cr(<<>>, S=#state{}) ->
    {more, {fun last_chunk_cr/2, S}};
last_chunk_cr(<<"\n", Remainder/binary>>, S=#state{length=0}) ->
    %% We're done! We ignore trailers and then return potentially bad data
    %% because of that, until trailers *are* supported.
    #state{buffer=Buf} = maybe_buffer(<<"\n">>, S),
    {done, Buf, Remainder};
last_chunk_cr(Bin, #state{length=0, buffer=Buf}) ->
    %% Sorry, trailer!
    {error, {bad_chunk, {0, [Buf,Bin]}}}.


maybe_buffer(_Data, S=#state{type=unchunked}) -> S;
maybe_buffer(Data, S=#state{buffer=Buf}) ->
    S#state{buffer = <<Buf/binary, Data/binary>>}.

