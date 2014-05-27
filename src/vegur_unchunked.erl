%%% Chunked decoding according to
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
-module(vegur_unchunked).
-export([next_chunk/1, next_chunk/2,
         stream_chunk/1, stream_chunk/2,
         all_chunks/1]).
-record(state, {buffer = [] :: iodata(),
                length :: non_neg_integer(),
                trailers = undefined,
                sub_state = undefined :: chunk_size_cr|data_cr|ext_cr}).

%% Parses a binary stream to get the next chunk in it.
next_chunk(Bin) -> next_chunk(Bin, undefined).

next_chunk(Bin, undefined) -> chunk_size(Bin, #state{});
next_chunk(Bin, {Fun,State=#state{}}) -> Fun(Bin, State).

%% Parses a binary stream to get chunk delimitation. The difference from
%% next_chunk/1-2 is that this will not accumulate data, but simply return it
%% at each iteration, allowing to return partial chunks and a continuation
%% to get more later on without interrupting the parsing.
%%
%% This allows to know where the end of a message is in a stream, without
%% keeping any data in memory longer than necessary.
stream_chunk(Bin) -> stream_chunk(Bin, undefined).

stream_chunk(Bin, undefined) -> stream_chunk(Bin, {fun chunk_size/2, #state{}});
stream_chunk(Bin, {Fun, State=#state{}}) ->
    case Fun(Bin, State) of
        {done, Buf, Rest} -> {done, Buf, Rest};
        {error, Reason} -> {error, Reason};
        {chunk, Buf, Rest} -> {chunk, Buf, Rest};
        {more, {NewFun, S=#state{buffer=Buf, length=Len}}} ->
            {more, Len, Buf, {NewFun,S#state{buffer=[]}}}
    end.

%% Fetch all the chunks in a binary stream, and error out if they're not
%% all there.
all_chunks(Bin) -> all_chunks(Bin, []).

all_chunks(Bin, Acc) ->
    try next_chunk(Bin) of
        {done, Buf, Rest} -> {done, [Acc, Buf], Rest};
        {error, Reason} -> {error, Acc, Reason};
        {more, _State} -> {error, Acc, incomplete};
        {chunk, Buf, Rest} -> all_chunks(Rest, [Acc, Buf])
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

%% For chunk sizes, we parse and accumulate the value in hex characters one
%% by one -- the lenght of the field isn't valid until we hit a CRLF or an
%% extension (;) -- without a need to do a lookahead or keep state in the
%% event someone sends hilariously long lengths across packet boundaries.
chunk_size(<<"\r\n", Rest/binary>>, S=#state{length=Len, buffer=Buf}) ->
    case Len of
        0 ->
            case Rest of
                <<"\r\n", Remainder/binary>> -> % body-ending CLRF
                    {done, Buf, Remainder};
                _ -> % 0-length chunk, skip-ahead
                    {done, Buf, Rest}
            end;
        undefined ->
            {error, {bad_chunk, no_length}};
        Len ->
            data(Rest, S)
    end;
chunk_size(<<"\n", _/binary>> = Bin, #state{sub_state=chunk_size_cr}=State) ->
    %% Got \n when last read byte was \r, consider the size read and continue
    chunk_size(<<"\r", Bin/binary>>, State#state{sub_state=undefined});
chunk_size(<<N, Rest/binary>>, S=#state{length=Len}) when N >= $0, N =< $9 ->
    NewLen = case Len of
        undefined -> N-$0;
        Len -> Len*16 + N-$0
    end,
    chunk_size(Rest, S#state{length=NewLen});
chunk_size(<<H, Rest/binary>>, S=#state{length=Len}) when H >= $A, H =< $F;
                                                          H >= $a, H =< $f ->
    N = if H >= $a, H =< $f -> H - $a + 10;
           true -> H - $A + 10
        end,
    NewLen = case Len of
        undefined -> N;
        Len -> Len*16 + N
    end,
    chunk_size(Rest, S#state{length=NewLen});
chunk_size(<<";", Rest/binary>>, S=#state{length=Len}) ->
    case Len of
        undefined -> {error, {bad_chunk, no_length}};
        _ -> extension(Rest, S)
    end;
chunk_size(<<>>, State) ->
    {more, {fun chunk_size/2, State}};
chunk_size(<<"\r">>, State) ->
    %% Got CR as the final byte in some input with length 0, this
    %% could mean that there is a \n waiting to be read which would
    %% make this valid. Mark sub_state, and ask for more.
    {more, {fun chunk_size/2, State#state{sub_state=chunk_size_cr}}};
chunk_size(<<BadChar, _/binary>>, _State) ->
    {error, {bad_chunk, {length_char, <<BadChar>>}}}.

extension(Bin, State) -> ext_name(Bin, State).

ext_name(<<"\r\n", Rest/binary>>, S=#state{}) ->
    data(Rest, S);
ext_name(<<"\n", Rest/binary>>, #state{sub_state=ext_cr}=State) ->
    %% Got \n when last read byte was \r, consider the ext name
    data(Rest, State#state{sub_state=undefined});
ext_name(<<>>, State) ->
    {more, {fun ext_name/2, State}};
ext_name(<<"=", Rest/binary>>, State) ->
    ext_val(Rest, State);
ext_name(<<"\r">>, State) ->
    %% Got CR as the final byte in some input with length 0, this
    %% could mean that there is a \n waiting to be read which would
    %% make this valid. Mark sub_state, and ask for more.
    {more, {fun ext_name/2, State#state{sub_state=ext_cr}}};
ext_name(<<_, Rest/binary>>, State) ->
    ext_name(Rest, State).

ext_val(<<"\r\n", Rest/binary>>, S=#state{}) ->
    data(Rest, S);
ext_val(<<"\n", Rest/binary>>, #state{sub_state=ext_cr}=State) ->
    %% Got \n when last read byte was \r, consider the ext value read
    data(Rest, State#state{sub_state=undefined});
ext_val(<<";", Rest/binary>>, State) ->
    extension(Rest, State);
ext_val(<<>>, State) ->
    {more, {fun ext_val/2, State}};
ext_val(<<"\"", Rest/binary>>, State) ->
    quoted_string(Rest, State);
ext_val(<<"\r">>, State) ->
    {more, {fun ext_val/2, State#state{sub_state=ext_cr}}};
ext_val(<<_, Rest/binary>>, State) ->
    ext_val(Rest, State).

quoted_string(<<"\\\"", Rest/binary>>, State) ->
    quoted_string(Rest, State);
quoted_string(<<"\"", Rest/binary>>, State) ->
    ext_val(Rest, State);
quoted_string(<<>>, State) ->
    {more, {fun quoted_string/2, State}};
quoted_string(<<_, Rest/binary>>, State) ->
    quoted_string(Rest, State).

data(<<"\r\n\r\n", Rest/binary>>, #state{length=0, buffer=Buf}) ->
    %% We had a last chunk (0-sized) but with a chunk-extension, and are RFC-
    %% specific for the last line of the chunked-body to contain an additional
    %% CRLF
    {done, Buf, Rest};
data(<<"\r\n", Rest/binary>>, #state{length=0, buffer=Buf}) ->
    %% We had a last chunk (0-sized) but with a chunk-extension, but allow
    %% some fudging where the last CRLF of the body isn't there.
    {done, Buf, Rest};
data(Bin, S=#state{length=Len, buffer=Buf}) when Len > 0 ->
    case Bin of
        <<Chunk:Len/binary, "\r\n", Rest/binary>> ->
            {chunk, [Buf, Chunk], Rest};
        <<>> ->
            {more, {fun data/2, S}};
        Bin when byte_size(Bin) > Len -> % we should fit here but we don't.
            {error, {bad_chunk, {Len, [Buf, Bin]}}};
        Bin -> %% Data possibly missing.
            {more, {fun data/2, S#state{length = Len-byte_size(Bin),
                                        buffer=[Buf,Bin]}}}
    end;
data(<<"\n", Rest/binary>>, #state{length=0, buffer=Buf,
                                   sub_state=data_cr}) ->
    %% Got \n when last read byte was \r, consider the chunk read
    {chunk, [Buf], Rest};
data(<<"\r">>, #state{length=0}=State) ->
    %% Got CR as the final byte in some input with length 0, this
    %% could mean that there is a \n waiting to be read which would
    %% make this valid. Mark sub_state, and ask for more.
    {more, {fun data/2, State#state{sub_state=data_cr}}}.



