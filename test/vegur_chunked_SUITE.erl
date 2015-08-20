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
-module(vegur_chunked_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [bad_length, html, short_msg, stream, trailers, bad_trailers,
          zero_crlf_end, boundary_chunk, zero_chunk_in_middle,
          bad_next_chunk, embedded_cr, all_bounds_1, all_bounds_2
         ].

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

bad_length(_) ->
    String = <<""
    "05\r\n"
    "this \r\n"
    "07\r\n"
    "string \r\n"
    "12\r\n"
    "is chunked encoded\r\n"
    "01\n\r\n" %% fail here, CRLF missing for length
    "00">>,
    {error, _, {bad_chunk, {length_char, <<"\n">>}}} = vegur_chunked:all_chunks(String),
    {chunk, _, Rest0} = vegur_chunked:next_chunk(String), % 05 CRLF this CRLF
    {chunk, _, Rest1} = vegur_chunked:next_chunk(Rest0), % 07 CRLF string CRLF
    {chunk, _, Rest2} = vegur_chunked:next_chunk(Rest1), % 12 is chunked ... CRLF
    {error, {bad_chunk, {length_char, <<"\n">>}}} = vegur_chunked:next_chunk(Rest2). % 01\n CRLF

short_msg(_) ->
    String = <<""
    "05\r\n"
    "this \r\n"
    "07\r\n"
    "string \r\n"
    "12\r\n"
    "is chunke">>,
    {error, _, incomplete} = vegur_chunked:all_chunks(String),
    {chunk, _, Rest0} = vegur_chunked:next_chunk(String), % 05 CRLF this CRLF
    {chunk, _, Rest1} = vegur_chunked:next_chunk(Rest0), % 07 CRLF string CRLF
    {more, Rest2} = vegur_chunked:next_chunk(Rest1), % 12 is chunked ... CRLF
    {more, Rest3} = vegur_chunked:next_chunk(<<"d en">>, Rest2),
    {chunk, _, <<"">>} = vegur_chunked:next_chunk(<<"coded\r\n">>, Rest3),
    {more, Rest4} = vegur_chunked:next_chunk(<<"00">>),
    {more, Rest5} = vegur_chunked:next_chunk(<<"\r\n">>, Rest4),
    {more, Rest5} = vegur_chunked:next_chunk(<<>>, Rest5),
    {more, _TrailerExpect} = vegur_chunked:next_chunk(<<"garbage">>, Rest5),
    {error, {bad_chunk, {bad_trailer,_}}} = vegur_chunked:next_chunk(<<" garbage">>, Rest5),
    {done, _, <<"">>} = vegur_chunked:next_chunk(<<"\r\n">>, Rest5),
    {done, _, <<"\r\n">>} = vegur_chunked:next_chunk(<<"\r\n\r\n">>, Rest5),
    {done, _, <<"">>} = vegur_chunked:next_chunk(<<"\r\n\r\n">>, Rest4).


html(_) ->
    String = <<""
    "c\r\n"
    "<h1>go!</h1>\r\n"
    "1b\r\n"
    "<h1>first chunk loaded</h1>\r\n"
    "2a\r\n"
    "<h1>second chunk loaded and displayed</h1>\r\n"
    "29\r\n"
    "<h1>third chunk loaded and displayed</h1>\r\n"
    "0\r\n">>,
    {error, _, incomplete} = vegur_chunked:all_chunks(String),
    S = byte_size(String),
    {done, Buf, <<>>} = vegur_chunked:all_chunks(<<String:S/binary, "\r\n">>),
    <<String:S/binary, "\r\n">> = iolist_to_binary(Buf).

stream(_) ->
    Str1 = <<""
    "c\r\n">>,
    Str2 = <<""
    "<h1>go!</h1>\r\n"
    "1b\r\n">>,
    Str3 = <<""
    "<h1>first chunk loaded</h1>\r\n">>,
    Str4 = <<""
    "2a\r\n"
    "<h1>second chunk">>,
    Str5 = <<" loaded and displayed</h1>\r\n"
    "29\r\n"
    "<h1>third chunk loaded and displayed</h1>\r\n"
    "0\r\n">>,
    %% remaining length is 12 given we haven't started parsing the message below
    {more, 12, Buf1, Cont1} = vegur_chunked:stream_chunk(Str1),
    {chunk, Buf2, Rest1} = vegur_chunked:stream_chunk(Str2, Cont1),
    {more, _, Buf3, Cont2} = vegur_chunked:stream_chunk(Rest1),
    {chunk, Buf4, Rest2} = vegur_chunked:stream_chunk(Str3, Cont2),
    %% here because we end on a chunk directly, there is no estimated length
    %% possible and we get 'undefined'
    {more, undefined, Buf5, Cont3} = vegur_chunked:stream_chunk(Rest2),
    {more, _, Buf6, Cont4} = vegur_chunked:stream_chunk(Str4, Cont3),
    {chunk, Buf7, Rest3} = vegur_chunked:stream_chunk(Str5, Cont4),
    {chunk, Buf8, Rest4} = vegur_chunked:stream_chunk(Rest3, undefined), % that works too
    {more, _, Buf9, Cont5} = vegur_chunked:stream_chunk(Rest4),
    {more, _, <<>>, Cont5} = vegur_chunked:stream_chunk(<<>>, Cont5),
    {done, <<"\r\n">>, <<>>} = vegur_chunked:stream_chunk(<<"\r\n">>, Cont5),
    true = iolist_to_binary([Buf1, Buf2, Buf3, Buf4, Buf5, Buf6, Buf7, Buf8, Buf9])
       =:= iolist_to_binary([Str1, Str2, Str3, Str4, Str5]).

trailers(_) ->
    %% The RFC Specifies that An HTTP/1.1 message SHOULD include a Trailer
    %% header field in a message using chunked transfer-coding with a
    %% non-empty trailer.
    %%
    %% Because a SHOULD is used, the header verification for trailers is purely
    %% optional.
    String= <<""
    "29\r\n"
    "<html><body><p>The file you requested is \r\n"
    "5\r\n"
    "3,400\r\n"
    "22\r\n"
    "bytes long and was last modified: \r\n"
    "1d\r\n"
    "Sat, 20 Mar 2004 21:12:00 GMT\r\n"
    "13\r\n"
    ".</p></body></html>\r\n"
    "0\r\n"
    "Expires: Sat, 27 Mar 2004 21:12:00 GMT\r\n"
    "Some-HEader: with\"a \\\" quoted\"value\r\n"
    " that-line-folded-its-value\r\n"
    "Last-Header: aaa\r\n"
    "\r\n">>,
    {done, Buf, <<>>} = vegur_chunked:all_chunks(String, undefined),
    {done, Buf2} = parse_chunked(String, undefined),
    String = iolist_to_binary(Buf),
    String = iolist_to_binary(Buf2).

bad_trailers(_) ->
    %% The RFC Specifies that An HTTP/1.1 message SHOULD include a Trailer
    %% header field in a message using chunked transfer-coding with a
    %% non-empty trailer.
    %%
    %% Because a SHOULD is used, the header verification for trailers is purely
    %% optional.
    String= <<""
    "29\r\n"
    "<html><body><p>The file you requested is \r\n"
    "5\r\n"
    "3,400\r\n"
    "22\r\n"
    "bytes long and was last modified: \r\n"
    "1d\r\n"
    "Sat, 20 Mar 2004 21:12:00 GMT\r\n"
    "13\r\n"
    ".</p></body></html>\r\n"
    "0\r\n"
    "Expires: Sat, 27 Mar 2004 21:12:00 GMT\r\n"
    "Some-HEader: with\"a \\\" quoted\"value\r\n"
    " that-line-folded-its-value\r\n"
    "Last-Header: aaa\r\n"  % bad trailer ending, missing an additional CLRF
    "GET some/path HTTP/1.1\r\n">>,
    {error, _Buf, {bad_chunk, {bad_trailer,_}}} = vegur_chunked:all_chunks(String, undefined),
    {error, {bad_chunk, {bad_trailer,_}}} = parse_chunked(String, undefined).


boundary_chunk(_) ->
    Chunks1 = <<""
    "c\r\n"
    "<h1>go!</h1>\r\n"
    "1b\r\n"
    "<h1>first chunk loaded</h1>\r\n"
    "2a\r\n"
    "<h1>second chunk loaded and displayed</h1>\r\n"
    "29\r\n"
    "<h1>third chunk loaded and displayed</h1>\r\n"
    "0\r\n">>,
    incomplete = parse_chunked(Chunks1, undefined),
    S = byte_size(Chunks1),
    {done, <<Chunks1:S/binary, "\r\n">>} = parse_chunked(<<Chunks1:S/binary, "\r\n">>, undefined),
    Chunks2 = <<""
   "c\r\n"
   "<h1>go!</h1>\r\n"
    "0\r\n\r\n">>,
    {done, Chunks2} = parse_chunked(Chunks2, undefined).


zero_crlf_end(_) ->
    String = <<""
    "c\r\n"
    "<h1>go!</h1>\r\n"
    "0\r\n\r\n">>,
    {done, Buf, <<>>} = vegur_chunked:all_chunks(String),
    String = iolist_to_binary(Buf).

zero_chunk_in_middle(_) ->
    B1 = <<""
    "c\r\n"
    "<h1>go!</h1>\r\n"
    "0\r\n">>,
    B2 = <<"c\r\n"
    "<h1>go!</h1>\r\n"
    "\r\n">>,
    B3 = <<"\r\n">>,
    {error, _, {bad_chunk,{bad_trailer,_}}} = vegur_chunked:all_chunks(<<B1/binary, B2/binary>>),
    {done, Buf, Rest} = vegur_chunked:all_chunks(<<B1/binary, B3/binary, B2/binary>>),
    S = byte_size(B1),
    <<B1:S/binary,B3/binary>> = iolist_to_binary(Buf),
    B2 = iolist_to_binary(Rest).

bad_next_chunk(_) ->
    String = <<""
    "c\r\r\n"
    "<h1>go!</h1>\r\n"
    "0\r\n">>,
    {error, [], {bad_chunk, {length_char, <<"\r">>}}} = vegur_chunked:all_chunks(String).

embedded_cr(_) ->
    String = <<""
    "c\r\n"
    "<h1>go!</h1>\r\n"
    "1c\r\n"
    "<h1>first chu\rnk loaded</h1>\r\n"
    "2b\r\n"
    "<h1>second chunk lo\raded and displayed</h1>\r\n"
    "2a\r\n"
    "<h1>third chunk loa\rded and displayed</h1>\r\n"
    "0\r\n\r\n">>,
    {done, Buf, <<>>} = vegur_chunked:all_chunks(String),
    String = iolist_to_binary(Buf).

all_bounds_1(_) ->
    S = <<""
    "c\r\n"
    "<h1>go!</h1>\r\n"
    "1c\r\n"
    "<h1>first chu\rnk loaded</h1>\r\n"
    "2b\r\n"
    "\r<h1>second chunk loaded and displayed</h1>\r\n"
    "2a\r\n"
    "<h1>third chunk loaded and displayed</h1>\r\r\n"
    "0\r\n\r\n">>,
    [begin
         S1 = binary:part(S, 0, N),
         S2 = binary:part(S, N, byte_size(S) - N),
         {more, Cont} = consume_chunks(S1, undefined),
         {done, <<"0\r\n\r\n">>, <<>>} = consume_chunks(S2, Cont)
     end
     || N <- lists:seq(1, byte_size(S) - 1)].

%% this probably repeats cases far more often than it needs too, but
%% it runs acceptably quickly on my machine (< 10s, ~3s for this
%% test), so I don't think it's worth the effort to clean it up more.
all_bounds_2(_) ->
    S = <<""
    "c\r\n"
    "<h1>go!</h1>\r\n"
    "1c\r\n"
    "<h1>first chu\rnk loaded</h1>\r\n"
    "2b\r\n"
    "\r<h1>second chunk loaded and displayed</h1>\r\n"
    "2a\r\n"
    "<h1>third chunk loaded and displayed</h1>\r\r\n"
    "0\r\n\r\n">>,
    [begin
         One = min(M, N),
         Two = max(M, N),
         S1 = binary:part(S, 0, One),
         S2 = binary:part(S, One, Two - One),
         S3 = binary:part(S, Two, byte_size(S) - Two),
         {more, Cont} = consume_chunks(S1, undefined),
         {more, Cont1} = consume_chunks(S2, Cont),
         {done, <<"0\r\n\r\n">>, <<>>} = consume_chunks(S3, Cont1)
     end
     || {N, M} <- [{I, O} || I <- lists:seq(1, byte_size(S) - 1),
                             O <- lists:seq(1, byte_size(S) - 1),
                             I /= O,
                             abs(I-O) /= 1 ]].

%%% helpers

consume_chunks(Bin, Cont) ->
    case vegur_chunked:next_chunk(Bin, Cont) of
        {chunk, _, Rest} ->
            consume_chunks(Rest, undefined);
        Else ->
            Else
    end.

parse_chunked(Buf, State) -> parse_chunked(State, Buf, State, <<>>).

parse_chunked(InitState, <<>>, State, Acc) ->
    parse(InitState, <<>>, <<>>, State, Acc);
parse_chunked(InitState, <<B:1/binary, Rest/binary>>, State, Acc) ->
    parse(InitState, B, Rest, State, Acc).

parse(InitState, What, Rest, State, Acc) ->
    case vegur_chunked:stream_chunk(What, State) of
        {done, Res, _Remainder} ->
            Bin = iolist_to_binary(Res),
            {done, <<Acc/binary, Bin/binary>>};
        {error, _Reason} = Res ->
            Res;
        {chunk, Chunk, MoreBuffer} ->
            Bin = iolist_to_binary(Chunk),
            parse_chunked(InitState, <<Rest/binary, MoreBuffer/binary>>, InitState, <<Acc/binary, Bin/binary>>);
        {more, _Len, Buf, State1} when Rest =/= <<>> ->
            Bin = iolist_to_binary(Buf),
            parse_chunked(InitState, Rest, State1, <<Acc/binary, Bin/binary>>);
        {more, _, _, _} when Rest =:= <<>> ->
            incomplete
    end.
