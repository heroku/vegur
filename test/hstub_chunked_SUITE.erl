-module(hstub_chunked_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [bad_length, html, short_msg, trailers].

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
    {error, _, {length_char, <<"\n">>}} = hstub_chunked:all_chunks(String),
    {chunk, _, Rest0} = hstub_chunked:next_chunk(String), % 05 CRLF this CRLF
    {chunk, _, Rest1} = hstub_chunked:next_chunk(Rest0), % 07 CRLF string CRLF
    {chunk, _, Rest2} = hstub_chunked:next_chunk(Rest1), % 12 is chunked ... CRLF
    {error, {length_char, <<"\n">>}} = hstub_chunked:next_chunk(Rest2). % 01\n CRLF

short_msg(_) ->
    String = <<""
    "05\r\n"
    "this \r\n"
    "07\r\n"
    "string \r\n"
    "12\r\n"
    "is chunke">>,
    {error, _, incomplete} = hstub_chunked:all_chunks(String),
    {chunk, _, Rest0} = hstub_chunked:next_chunk(String), % 05 CRLF this CRLF
    {chunk, _, Rest1} = hstub_chunked:next_chunk(Rest0), % 07 CRLF string CRLF
    {more, Rest2} = hstub_chunked:next_chunk(Rest1), % 12 is chunked ... CRLF
    {more, Rest3} = hstub_chunked:next_chunk(<<"d en">>, Rest2),
    {chunk, _, <<"">>} = hstub_chunked:next_chunk(<<"coded\r\n">>, Rest3),
    {more, Rest4} = hstub_chunked:next_chunk(<<"00">>),
    {done, _, <<"">>} = hstub_chunked:next_chunk(<<"\r\n">>, Rest4),
    {done, _, <<"">>} = hstub_chunked:next_chunk(<<"\r\n\r\n">>, Rest4).

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
    {done, Buf, <<>>} = hstub_chunked:all_chunks(String),
    String = iolist_to_binary(Buf).

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
    {done, Buf, <<>>} = hstub_chunked:all_chunks(String, <<"Expires">>),
    String = iolist_to_binary(Buf).
