-module(vegur_chunked_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [bad_length, html, short_msg, stream, trailers].

init_per_testcase(trailers, _) ->
    {skip, "Trailers not supported"};
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
    {error, _, {length_char, <<"\n">>}} = vegur_chunked:all_chunks(String),
    {chunk, _, Rest0} = vegur_chunked:next_chunk(String), % 05 CRLF this CRLF
    {chunk, _, Rest1} = vegur_chunked:next_chunk(Rest0), % 07 CRLF string CRLF
    {chunk, _, Rest2} = vegur_chunked:next_chunk(Rest1), % 12 is chunked ... CRLF
    {error, {length_char, <<"\n">>}} = vegur_chunked:next_chunk(Rest2). % 01\n CRLF

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
    {done, _, <<"">>} = vegur_chunked:next_chunk(<<"\r\n">>, Rest4),
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
    {done, Buf, <<>>} = vegur_chunked:all_chunks(String),
    String = iolist_to_binary(Buf).

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
    {done, Buf9, <<>>} = vegur_chunked:stream_chunk(Rest4),
    true = iolist_to_binary([Buf1, Buf2, Buf3, Buf4, Buf5, Buf6, Buf7, Buf8, Buf9])
       =:= iolist_to_binary([Str1, Str2, Str3, Str4, Str5]).

trailers(_) ->
    String= <<""
    "29\r\n"
    "<html><body><p>The file you requested is\r\n"
    "5\r\n"
    "3,400\r\n"
    "23\r\n"
    "bytes long and was last modified:\r\n"
    "1d\r\n"
    "Sat, 20 Mar 2004 21:12:00 GMT\r\n"
    "13\r\n"
    ".</p></body></html>\r\n"
    "0\r\n"
    "Expires: Sat, 27 Mar 2004 21:12:00 GMT\r\n">>,
    {done, Buf, <<>>} = vegur_chunked:all_chunks(String, <<"Expires">>),
    String = iolist_to_binary(Buf).
