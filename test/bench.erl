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
-module(bench).

-compile(export_all).

-define(CRLF, <<"\r\n">>).

bench(Chunks) ->
    bench(Chunks, false).

bench(Chunks, Profile) ->
    erlang:garbage_collect(),
    {Time, _} =
        timer:tc(fun() ->
                         if Profile ->
                                 eprof:start_profiling([self()]);
                            true -> ok
                         end,
                         [{done, _, _} =
                              vegur_chunked:all_chunks(Chunk)
                          || Chunk <- Chunks],
                         if Profile ->
                                 eprof:stop_profiling(),
                                 eprof:analyze(total);
                            true -> ok
                         end
                 end),
    Time / 100000.

make_chunks(N, Len, ChunkLen) ->
    [make_chunked(Len, ChunkLen)
     || _ <- lists:seq(1, N)].


make_chunked(Len, ChunkLen) ->
    rand:seed(erlang:timestamp()),
    Chunks =
        [make_chunk(ChunkLen)
         || _ <- lists:seq(1, Len)],
    iolist_to_binary([Chunks, "0", ?CRLF, ?CRLF]).

make_chunk(Len) ->
    Data = rand_str(Len),
    LenS = io_lib:format("~.16B",[Len]),
   [LenS, ?CRLF, Data, ?CRLF].

rand_str(Len) ->
    [rand_letter() || _ <- lists:seq(1, Len)].

make_strs(N, L) ->
    [rand_str(L)
     || _ <- lists:seq(1, N)].

rand_letter() ->
    case rand:uniform(2) of
        1 ->
            rand:uniform($z - $a) + $a;
        _ ->
            rand:uniform($Z - $A) + $A
    end.
