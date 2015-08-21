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
%%%
%%% @doc
%%% Chunked encoding delimitation according to
%%% [http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.6.1]
%%% ```
%%%      Chunked-Body   = *chunk
%%%                       last-chunk
%%%                       trailer
%%%                       CRLF
%%%
%%%      chunk          = chunk-size [ chunk-extension ] CRLF
%%%                       chunk-data CRLF
%%%      chunk-size     = 1*HEX
%%%      last-chunk     = 1*("0") [ chunk-extension ] CRLF
%%%
%%%      chunk-extension= *( ";" chunk-ext-name [ "=" chunk-ext-val ] )
%%%      chunk-ext-name = token
%%%      chunk-ext-val  = token | quoted-string
%%%      chunk-data     = chunk-size(OCTET)
%%%      trailer        = *(entity-header CRLF)
%%% '''
%%%
%%% For brevity in parsing, we consider 'token' to be any character not to
%%% be a delimiter, whereas the RFC defines them as
%%% `1*<any CHAR except CTLs or separators>', where CTL is defined as
%%% (octets 0 - 31) and DEL (127).
%%%
%%% We also do not support extensions and will discard them:
%%
%% All HTTP/1.1 applications MUST be able to receive and decode the "chunked"
%% transfer-coding, and MUST ignore chunk-extension extensions they do not
%% understand.
%% @end
-module(vegur_chunked).
-export([next_chunk/1, next_chunk/2,
         next_unchunk/1, next_unchunk/2,
         stream_chunk/1, stream_chunk/2,
         stream_unchunk/1, stream_unchunk/2,
         all_chunks/1, all_chunks/2, all_unchunks/1]).
-record(state, {buffer = <<>> :: binary(),
                length :: non_neg_integer()|undefined,
                type = chunked :: chunked|unchunked}).

%% Parses a binary stream to get the next chunk in it.
next_unchunk(Bin) -> next_unchunk(Bin, undefined).

next_unchunk(Bin, undefined) ->
    chunk_size(Bin, #state{type=unchunked});
next_unchunk(Bin, {_, #state{}}=S) -> next_chunk(Bin, S).

next_chunk(Bin) -> next_chunk(Bin, undefined).

next_chunk(Bin, undefined) ->
    chunk_size(Bin, #state{type=chunked});
next_chunk(Bin, {Fun,State=#state{}}) ->
    Fun(Bin, State).

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

stream_chunk(Bin, undefined) ->
    stream_chunk(Bin, {fun chunk_size/2, #state{type=chunked}});
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

all_chunks(Bin) -> all_chunks(Bin, undefined).

all_chunks(Bin, Opt) -> all_chunks(Bin, fun(Arg) -> next_chunk(Arg, Opt) end, []).

all_chunks(Bin, F, Acc) ->
    try F(Bin) of
        {done, Buf, Rest} -> {done, [Acc, Buf], Rest};
        {error, Reason} -> {error, Acc, Reason};
        {more, _State} -> {error, Acc, incomplete};
        {chunk, Buf, Rest} -> all_chunks(Rest, F, [Acc, Buf])
    catch
        error:function_clause -> {error, Acc, {bad_chunk, Bin}}
    end.

%%           ,-----------------v
%% chunk_size -> extension -> data -> last chunk -> CRLF
%%                |    ^                 |     __    |
%%                V    |                 |    |  |   |
%%          ext_name -> ext_val          '-> trailer-'
%%                        | |                 |  |
%%                    quoted string         header_name
%%                                            |  |
%%                                          header_value
%%                                            |  |
%%                                         quoted string
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
    maybe_trailers(Rest, maybe_buffer(<<"\n">>, S));
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
ext_quoted_string(<<"\\", Rest/binary>>, S=#state{}) ->
    ext_quoted_string_esc(Rest, S);
ext_quoted_string(<<"\"", Rest/binary>>, S=#state{}) ->
    ext_val(Rest, S);
ext_quoted_string(<<_, Rest/binary>>, S=#state{}) ->
    ext_quoted_string(Rest, S).

ext_quoted_string_esc(<<>>, S=#state{}) ->
    {more, {fun ext_quoted_string_esc/2, S}};
ext_quoted_string_esc(<<_, Rest/binary>>, S=#state{}) ->
    ext_quoted_string(Rest, S).

%% instead of buffering byte by byte, keep a reference to the original
%% binary, iterate through using a cursor, and only buffer (hopefully
%% larger) binaries when you must.
chunk_data(Bin, S=#state{}) ->
    Sz = byte_size(Bin),
    %% binaries count from 0
    chunk_data(Bin, S#state{}, 0, Sz).

%% read beyond the anticipated end of the chunk, error out
chunk_data(Bin, #state{length=L, buffer=Buf}, P, _len) when P > L ->
    {error, {bad_chunk, {0, [Buf,Bin]}}};
%% P = L  means we've run out of binary without a match, go back for
%% more data.
chunk_data(Bin, S=#state{buffer=Buf, length=Len}, P, P) ->
    Bin1 = <<Buf/binary, Bin/binary>>,
    {more, {fun chunk_data/2, S#state{buffer=Bin1,length=Len-P}}};
%% otherwise, look for \r
chunk_data(Bin, S=#state{buffer=Buf, length=Len}, P, L) ->
    case binary:at(Bin, P) of
        $\r when Len =:= P ->
            %% add one because at/2 is 0-indexed
            Pos = P + 1,
            <<Bin1:Pos/binary, Rest/binary>> = Bin,
            Buf1 = <<Buf/binary, Bin1/binary>>,
            chunk_data_cr(Rest, S#state{buffer=Buf1,
                                        length=Len-P});
        _ ->
            chunk_data(Bin, S, P + 1, L)
    end.

chunk_data_cr(<<>>, S=#state{}) ->
    {more, {fun chunk_data_cr/2, S}};
chunk_data_cr(<<"\n", Rest/binary>>, #state{type=chunked,
                                            buffer=Buf}) ->
    {chunk, <<Buf/binary, "\n">>, Rest};
chunk_data_cr(<<"\n", Rest/binary>>, #state{type=unchunked,
                                            buffer=Buf}) ->
    %% have to go back and clip the end off of this because we carry
    %% it through no matter what in chunk_data/4
    Buf1 =
        case catch binary:last(Buf) of
            $\r ->
                binary:part(Buf, 0, byte_size(Buf) - 1);
            %% catch here will usually mean <<>>, which it doesn't
            %% matter if we pass through.
            _ ->
                Buf
        end,
    {chunk, Buf1, Rest};
chunk_data_cr(Bin, #state{buffer=Buf, length=Len}) ->
    {error, {bad_chunk, {Len, [Buf,Bin]}}}.

%% We're at the end of the stream, potentially, after
%% a 0-length chunk. We wait forever on incomplete
%% streams.
maybe_trailers(<<>>, S=#state{}) ->
    {more, {fun maybe_trailers/2, S}};
maybe_trailers(<<"\r", _/binary>> = Data, S=#state{}) ->
    finalize(Data, S);
maybe_trailers(OtherData, S=#state{}) ->
    trailer(OtherData, S).

%% Passthrough. header_name -> header_value -> switch
trailer(Bin, S=#state{}) ->
    header_name(Bin, S).

header_name(<<>>, S=#state{}) ->
    {more, {fun header_name/2, S}};
header_name(<<":", Rest/binary>>, S=#state{}) ->
    %% end of header, start the value
    header_value(Rest, maybe_buffer(<<":">>, S));
header_name(<<Char, Rest/binary>> = Bin, S=#state{buffer=Buf}) ->
    case class(Char) of
        token ->
            header_name(Rest, maybe_buffer(<<Char>>, S));
        _ ->
            %% That's an invalid trailer and/or an improperly terminated
            %% chunked session that also uses pipelining. Gods have mercy.
            {error, {bad_chunk, {bad_trailer, [Buf, Bin]}}}
    end.

header_value(<<>>, S=#state{}) ->
    {more, {fun header_value/2, S}};
header_value(<<"\r", Rest/binary>>, S=#state{}) ->
    header_value_cr(Rest, maybe_buffer(<<"\r">>, S));
header_value(<<"\"", Rest/binary>>, S=#state{}) ->
    header_value_quoted_string(Rest, maybe_buffer(<<"\"">>, S));
header_value(<<Char, Rest/binary>>, S=#state{}) ->
    header_value(Rest, maybe_buffer(<<Char>>, S)).

header_value_cr(<<"">>, S=#state{}) ->
    {more, {fun header_value_cr/2, S}};
header_value_cr(<<"\n", Rest/binary>>, S=#state{}) ->
    header_value_switch(Rest, maybe_buffer(<<"\n">>, S));
header_value_cr(Bin, #state{buffer=Buf}) ->
    {error, {bad_chunk, {bad_trailer, [Buf,Bin]}}}.

header_value_switch(<<"">>, S=#state{}) ->
    {more, {fun header_value_switch/2, S}};
header_value_switch(<<" ", Rest/binary>>, S=#state{}) ->
    %% Header folding
    header_value(Rest, maybe_buffer(<<" ">>, S));
header_value_switch(<<"\t", Rest/binary>>, S=#state{}) ->
    %% Header folding
    header_value(Rest, maybe_buffer(<<"\t">>, S));
header_value_switch(<<"\r", Rest/binary>>, S=#state{}) ->
    %% Last header
    finalize(<<"\r", Rest/binary>>, S);
header_value_switch(Bin, S=#state{}) ->
    %% Other trailer: default
    trailer(Bin, S).

header_value_quoted_string(<<>>, S=#state{}) ->
    {more, {fun header_value_quoted_string/2, S}};
header_value_quoted_string(<<"\\", Rest/binary>>, S=#state{}) ->
    header_value_quoted_string_esc(Rest, maybe_buffer(<<"\\">>, S));
header_value_quoted_string(<<"\"", Rest/binary>>, S=#state{}) ->
    header_value(Rest, maybe_buffer(<<"\"">>, S));
header_value_quoted_string(<<Char, Rest/binary>>, S=#state{}) ->
    header_value_quoted_string(Rest, maybe_buffer(<<Char>>, S)).

header_value_quoted_string_esc(<<>>, S=#state{}) ->
    {more, {fun header_value_quoted_string_esc/2, S}};
header_value_quoted_string_esc(<<Char, Rest/binary>>, S=#state{}) ->
    header_value_quoted_string(Rest, maybe_buffer(<<Char>>, S)).

finalize(<<"\r",Rest/binary>>, S=#state{}) ->
    finalize_cr(Rest, maybe_buffer(<<"\r">>, S));
finalize(OtherData, #state{buffer=Buf}) ->
    %% That data may belong to anything, even other requests. Take
    %% no chances. We should have already checked for trailers at this point.
    {error, {bad_chunk, {last_crlf, [Buf,OtherData]}}}.

finalize_cr(<<>>, S=#state{}) ->
    {more, {fun finalize_cr/2, S}};
finalize_cr(<<"\n", Rest/binary>>, S=#state{}) ->
    #state{buffer=Buf} = maybe_buffer(<<"\n">>, S),
    {done, Buf, Rest};
finalize_cr(Bin, #state{length=0, buffer=Buf}) ->
    {error, {bad_chunk, {0, [Buf,Bin]}}}.

maybe_buffer(_Data, S=#state{type=unchunked}) -> S;
maybe_buffer(Data, S=#state{buffer=Buf}) ->
    S#state{buffer = <<Buf/binary, Data/binary>>}.

class($\s) -> separator;
class($\t) -> separator;
class($\\) -> separator;
class($() -> separator;
class($)) -> separator;
class($<) -> separator;
class($>) -> separator;
class($@) -> separator;
class($,) -> separator;
class($;) -> separator;
class($:) -> separator;
class($/) -> separator;
class($[) -> separator;
class($]) -> separator;
class($?) -> separator;
class($=) -> separator;
class(${) -> separator;
class($}) -> separator;
class(C) when C >= 0, C =< 31; C =:= 127 -> control;
class(_) -> token.
