-module(vegur_unchunked_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [bad_length, html, short_msg, stream, boundary_chunk, zero_crlf_end].

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
    {error, _, {bad_chunk, {length_char, <<"\n">>}}} = vegur_chunked:all_unchunks(String),
    {chunk, _, Rest0} = vegur_chunked:next_unchunk(String), % 05 CRLF this CRLF
    {chunk, _, Rest1} = vegur_chunked:next_unchunk(Rest0), % 07 CRLF string CRLF
    {chunk, _, Rest2} = vegur_chunked:next_unchunk(Rest1), % 12 is chunked ... CRLF
    {error, {bad_chunk, {length_char, <<"\n">>}}} = vegur_chunked:next_unchunk(Rest2). % 01\n CRLF

short_msg(_) ->
    String = <<""
    "05\r\n"
    "this \r\n"
    "07\r\n"
    "string \r\n"
    "12\r\n"
    "is chunke">>,
    {error, _, incomplete} = vegur_chunked:all_unchunks(String),
    {chunk, _, Rest0} = vegur_chunked:next_unchunk(String), % 05 CRLF this CRLF
    {chunk, _, Rest1} = vegur_chunked:next_unchunk(Rest0), % 07 CRLF string CRLF
    {more, Rest2} = vegur_chunked:next_unchunk(Rest1), % 12 is chunked ... CRLF
    {more, Rest3} = vegur_chunked:next_unchunk(<<"d en">>, Rest2),
    {chunk, _, <<"">>} = vegur_chunked:next_unchunk(<<"coded\r\n">>, Rest3),
    {more, Rest4} = vegur_chunked:next_unchunk(<<"00">>),
    {done, _, <<"">>} = vegur_chunked:next_unchunk(<<"\r\n\r\n">>, Rest4).

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
    {done, Buf, <<>>} = vegur_chunked:all_unchunks(String),
    <<"<h1>go!</h1>"
      "<h1>first chunk loaded</h1>"
      "<h1>second chunk loaded and displayed</h1>"
      "<h1>third chunk loaded and displayed</h1>">> = iolist_to_binary(Buf).

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
    {more, 12, Buf1, Cont1} = vegur_chunked:stream_unchunk(Str1),
    {chunk, Buf2, Rest1} = vegur_chunked:stream_unchunk(Str2, Cont1),
    {more, _, Buf3, Cont2} = vegur_chunked:stream_unchunk(Rest1),
    {chunk, Buf4, Rest2} = vegur_chunked:stream_unchunk(Str3, Cont2),
    %% here because we end on a chunk directly, there is no estimated length
    %% possible and we get 'undefined'
    {more, undefined, Buf5, Cont3} = vegur_chunked:stream_unchunk(Rest2),
    {more, _, Buf6, Cont4} = vegur_chunked:stream_unchunk(Str4, Cont3),
    {chunk, Buf7, Rest3} = vegur_chunked:stream_unchunk(Str5, Cont4),
    {chunk, Buf8, Rest4} = vegur_chunked:stream_unchunk(Rest3, undefined), % that works too
    {maybe_done, Buf9, Cont5} = vegur_chunked:stream_unchunk(Rest4),
    {done, <<>>, <<>>} = vegur_chunked:stream_unchunk(undefined, Cont5),
    true = iolist_to_binary([Buf1, Buf2, Buf3, Buf4, Buf5, Buf6, Buf7, Buf8, Buf9])
       =:= <<"<h1>go!</h1>"
             "<h1>first chunk loaded</h1>"
             "<h1>second chunk loaded and displayed</h1>"
             "<h1>third chunk loaded and displayed</h1>">>.

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
    done = parse_chunked(Chunks1, undefined),
    Chunks2 = <<""
   "c\r\n"
   "<h1>go!</h1>\r\n"
    "0\r\n\r\n">>,
    done = parse_chunked(Chunks2, undefined).


zero_crlf_end(_) ->
    String = <<""
    "c\r\n"
    "<h1>go!</h1>\r\n"
    "0\r\n\r\n">>,
    {done, Buf, <<>>} = vegur_chunked:all_unchunks(String),
    <<"<h1>go!</h1>">> = iolist_to_binary(Buf).

parse_chunked(<<>>, State) ->
    parse(<<>>, <<>>, State);
parse_chunked(<<B:1/binary, Rest/binary>>, State) ->
    parse(B, Rest, State).


parse(What, Rest, State) ->
    case vegur_chunked:stream_unchunk(What, State) of
        {done, _, _} ->
            done;
        {maybe_done, _, State1} ->
            parse_chunked(Rest, State1);
        {error, _Reason} = Res ->
            Res;
        {chunk, _Chunk, MoreBuffer} ->
            parse_chunked(<<Rest/binary, MoreBuffer/binary>>, undefined);
        {more, _, _, State1} ->
            parse_chunked(Rest, State1)
    end.
