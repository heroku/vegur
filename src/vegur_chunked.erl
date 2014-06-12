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
%%
%%% Note that this module only returns the byte sequences for chunks, and does
%%% not return decoded chunks for final consumption, but only chunk sequences
%%% useful for proxying and delimiting requests.
-module(vegur_chunked).
-export([next_chunk/1, next_chunk/2,
         next_unchunk/1, next_unchunk/2,
         stream_chunk/1, stream_chunk/2,
         stream_unchunk/1, stream_unchunk/2,
         all_chunks/1, all_unchunks/1]).
-record(state, {buffer = [] :: iodata(),
                length :: non_neg_integer()|undefined,
                trailers = undefined,
                sub_state = undefined :: undefined|chunk_size|chunk_size_cr|ext|
                                         ext_cr|data_cr|final_crlf|data,
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
            {more, Len, Buf, {NewFun,S#state{buffer=[]}}}
    end.

%% Fetch all the chunks in a binary stream, and error out if they're not
%% all there.
all_unchunks(Bin) -> all_chunks(Bin, fun next_unchunk/1, []).
    
all_chunks(Bin) -> all_chunks(Bin, fun next_chunk/1, []).

all_chunks(Bin, F, Acc) ->
    try F(Bin) of
        {done, Buf, Rest} -> {done, [Acc, Buf], Rest};
        {error, Reason} -> {error, Acc, Reason};
        {more, _State} -> {error, Acc, incomplete};
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

%% For chunk sizes, we parse and accumulate the value in hex characters one
%% by one -- the lenght of the field isn't valid until we hit a CRLF or an
%% extension (;) -- without a need to do a lookahead or keep state in the
%% event someone sends hilariously long lengths across packet boundaries.
chunk_size(<<"\r\n", Rest/binary>>, S=#state{sub_state=chunk_size, type=Type,
                                             buffer=Buf}) ->
    handle_chunk(Rest, S#state{buffer=update_buffer(Type, <<"\r\n">>, Buf)});
chunk_size(<<"\n", Rest/binary>>, #state{sub_state=chunk_size_cr, type=Type,
                                         buffer=Buf}=State) ->
    %% Last received byte was \r, and we are waiting for this \n. We now consider the
    %% chunk size read and continue.
    handle_chunk(Rest, State#state{buffer=update_buffer(Type, <<"\n">>, Buf)});
chunk_size(<<N, Rest/binary>>, S=#state{length=Len, buffer=Buf,
                                        type=Type}) when N >= $0, N =< $9 ->
    NewLen = case Len of
        undefined -> N-$0;
        Len -> Len*16 + N-$0
    end,
    chunk_size(Rest, S#state{length=NewLen, buffer=update_buffer(Type, N, Buf),
                             sub_state=chunk_size});
chunk_size(<<H, Rest/binary>>, S=#state{length=Len, buffer=Buf,
                                       type=Type}) when H >= $A, H =< $F;
                                                        H >= $a, H =< $f ->
    N = if H >= $a, H =< $f -> H - $a + 10;
           true -> H - $A + 10
        end,
    NewLen = case Len of
        undefined -> N;
        Len -> Len*16 + N
    end,
    chunk_size(Rest, S#state{length=NewLen, buffer = update_buffer(Type, H, Buf),
                             sub_state=chunk_size});
chunk_size(<<";", Rest/binary>>, S=#state{length=Len, sub_state=chunk_size}) ->
    case Len of
        undefined -> {error, {bad_chunk, no_length}};
        _ -> extension(Rest, S)
    end;
chunk_size(<<>>, State) ->
    {more, {fun chunk_size/2, State}};
chunk_size(<<"\r">>, #state{buffer=Buf, sub_state=chunk_size, type=Type}=State) ->
    %% Got CR as the final byte in some input. This could mean that there is a
    %% \n waiting to be read which would make this a valid chunk size.
    %% Ask for more and mark sub_state as chunk_size_cr
    {more, {fun chunk_size/2, State#state{buffer=update_buffer(Type, <<"\r">>, Buf),
                                          sub_state=chunk_size_cr}}};
chunk_size(<<"\r\n", Remainder/binary>>, #state{buffer=Buf, sub_state=undefined, type=Type}) ->
    %% The last chunk had an ending like 0\r\n\r\n but not all the data was read
    %% and it was therefore not possible to mark it as "done".
    {done, update_buffer(Type, <<"\r\n">>, Buf), Remainder};
chunk_size(<<"\r">>, #state{buffer=Buf, sub_state=undefined, type=Type}=State) ->
    {more, {fun chunk_size/2, State#state{buffer=update_buffer(Type, <<"\r">>, Buf),
                                          sub_state=final_crlf}}};
chunk_size(<<"\n", Remainder/binary>>, #state{buffer=Buf, sub_state=final_crlf, type=Type}) ->
    {done, update_buffer(Type, <<"\n">>, Buf), Remainder};
chunk_size(<<BadChar, _/binary>>, _State) ->
    {error, {bad_chunk, {length_char, <<BadChar>>}}}.
extension(Bin, State) -> ext_name(Bin, State#state{sub_state=ext}).

ext_name(<<"\r\n", Rest/binary>>, S=#state{buffer=Buf, sub_state=ext, type=Type}) ->
    data(Rest, S#state{buffer=update_buffer(Type, <<"\r\n">>, Buf), sub_state=data});
ext_name(<<"\n", Rest/binary>>, #state{buffer=Buf, type=Type,
                                       sub_state=ext_cr}=State) ->
    %% Got a \n when the last read byte was \r. Consider the ext name
    %% read and move on.
    data(Rest, State#state{buffer=update_buffer(Type, <<"\n">>, Buf),
                           sub_state=data});
ext_name(<<>>, #state{sub_state=ext}=State) ->
    {more, {fun ext_name/2, State}};
ext_name(<<"=", Rest/binary>>, #state{sub_state=ext}=State) ->
    ext_val(Rest, State);
ext_name(<<"\r">>, #state{buffer=Buf, sub_state=ext, type=Type}=State) ->
    %% Got a CR as the final byte, this could mean that there is a \n
    %% waiting to be read which would make this a valid extension name.
    %% Mark sub_state, ask for more and move on.
    {more, {fun ext_name/2, State#state{buffer=update_buffer(Type, <<"\r">>, Buf),
                                        sub_state=ext_cr}}};
ext_name(<<_, Rest/binary>>, #state{sub_state=ext} = State) ->
    ext_name(Rest, State).

ext_val(<<"\r\n", Rest/binary>>, S=#state{buffer=Buf, sub_state=ext, type=Type}) ->
    data(Rest, S#state{buffer=update_buffer(Type, <<"\r\n">>, Buf), sub_state=data});
ext_val(<<"\n", Rest/binary>>, #state{buffer=Buf, type=Type,
                                      sub_state=ext_cr}=State) ->
    %% Got \n when last read byte was \r, consider the ext value read
    data(Rest, State#state{buffer=update_buffer(Type, <<"\n">>, Buf),
                           sub_state=data});
ext_val(<<";", Rest/binary>>, #state{sub_state=ext}=State) ->
    extension(Rest, State);
ext_val(<<>>, #state{sub_state=ext}=State) ->
    {more, {fun ext_val/2, State}};
ext_val(<<"\"", Rest/binary>>, #state{sub_state=ext}=State) ->
    quoted_string(Rest, State);
ext_val(<<"\r">>, #state{buffer=Buf, sub_state=ext, type=Type}=State) ->
    {more, {fun ext_val/2, State#state{buffer=update_buffer(Type, <<"\r">>, Buf),
                                       sub_state=ext_cr}}};
ext_val(<<_, Rest/binary>>, #state{sub_state=ext}=State) ->
    ext_val(Rest, State).

quoted_string(<<"\\\"", Rest/binary>>, State) ->
    quoted_string(Rest, State);
quoted_string(<<"\"", Rest/binary>>, State) ->
    ext_val(Rest, State);
quoted_string(<<>>, State) ->
    {more, {fun quoted_string/2, State}};
quoted_string(<<_, Rest/binary>>, State) ->
    quoted_string(Rest, State).

data(<<"\r\n\r\n", Rest/binary>>, #state{length=0, buffer=Buf, sub_state=data,
                                         type=Type}) ->
    %% We had a last chunk (0-sized) but with a chunk-extension, and are RFC-
    %% specific for the last line of the chunked-body to contain an additional
    %% CRLF
    {done, update_buffer(Type, <<"\r\n\r\n">>, Buf), Rest};
data(<<"\r\n", Rest/binary>>, #state{length=0, buffer=Buf, sub_state=data,
                                    type=Type}) ->
    %% We had a last chunk (0-sized) but with a chunk-extension, but allow
    %% some fudging where the last CRLF of the body isn't there.
    {done, update_buffer(Type, <<"\r\n">>, Buf), Rest};
data(Bin, S=#state{length=Len, buffer=Buf, sub_state=data, type=Type}) when Len > 0 ->
    case Bin of
        <<Chunk:Len/binary, "\r\n", Rest/binary>> ->
            {chunk, update_buffer(Type, <<"\r\n">>, [Buf, Chunk]), Rest};
        <<>> ->
            {more, {fun data/2, S}};
        Bin when byte_size(Bin) > Len -> % we should fit here but we don't.
            {error, {bad_chunk, {Len, [Buf, Bin]}}};
        Bin -> %% Data possibly missing.
            {more, {fun data/2, S#state{length = Len-byte_size(Bin),
                                        buffer=[Buf,Bin]}}}
    end;
data(<<"\n", Rest/binary>>, #state{length=0, buffer=Buf, type=Type,
                                   sub_state=data_cr}) ->
    %% Got \n when last read byte was \r, consider the chunk read
    {chunk, update_buffer(Type, <<"\n">>, Buf), Rest};
data(<<"\r">>, #state{length=0, buffer=Buf, sub_state=data, type=Type}=State) ->
    %% Got CR as the final byte when the chunk length is 0, this could
    %% mean that there is a \n waiting to be read which would make this a
    %% valid chunk. Mark sub_state and ask for more.
    {more, {fun data/2, State#state{buffer=update_buffer(Type, <<"\r">>, Buf),
                                    sub_state=data_cr}}};
data(Bin, #state{length=0, buffer=Buf}) ->
    %% We have more data than the length would tell us.
    %% Bad chunk.
    {error, {bad_chunk, {0, [Buf,Bin]}}}.

handle_chunk(Bin, #state{length=Len, buffer=Buf, type=Type} = State) ->
    case Len of
        0 ->
            case Bin of
                <<"\r\n", Remainder/binary>> -> % body-ending CLRF
                    {done, update_buffer(Type, <<"\r\n">>, Buf), Remainder};
                _ -> % 0-length chunk
                    {done, Buf, Bin}
            end;
        undefined ->
            {error, {bad_chunk, no_length}};
        Len ->
            data(Bin, State#state{sub_state=data})
    end.

update_buffer(chunked, Bin, Buf) ->
    [Buf, Bin];
update_buffer(unchunked, _Bin, Buf) ->
    Buf.
