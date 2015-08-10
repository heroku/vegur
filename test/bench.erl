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
    random:seed(now()),
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
    case random:uniform(2) of
        1 ->
            random:uniform($z - $a) + $a;
        _ ->
            random:uniform($Z - $A) + $A
    end.
