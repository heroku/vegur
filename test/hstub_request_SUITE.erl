-module(hstub_request_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, hstub_request_handling}].

groups() ->
    [
     {hstub_request_handling, [], [no_host
                                   ,empty_host
                                   ,invalid_expect
                                  ]}
    ].

init_per_suite(Config) ->
    {ok, Cowboy} = application:ensure_all_started(cowboy),
    {ok, Inets} = application:ensure_all_started(inets),
    application:load(hstub),
    [{started, Cowboy++Inets} | Config].

end_per_suite(Config) ->
    [application:stop(App) || App <- lists:reverse(?config(started, Config))],
    ok.

init_per_group(hstub_request_handling, Config) ->
    HstubPort = 9333,
    application:set_env(hstub, http_listen_port, HstubPort),
    {ok, _} = application:ensure_all_started(hstub),
    [{hstub_port, HstubPort} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(hstub_request_handling, Config) ->
    application:stop(hstub),
    Config;
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(no_host, Config) ->
    Config;
init_per_testcase(empty_host, Config) ->
    Config;
init_per_testcase(invalid_expect, Config) ->
    Config;
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

%%%===================================================================
%%% Test cases
%%%===================================================================
no_host(Config) ->
    % Send a request with no Host header, expect 400 back. HTTPc doesn't allow me to do
    % invalid requests, so this one manual.
    Port = ?config(hstub_port, Config),
    Req = "GET / HTTP/1.1\r\n\r\n",
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket, Req),
    Data = get_until_closed(Socket, <<>>),
    M = binary:match(Data, <<"400">>),
    true = is_tuple(M),
    Config.

empty_host(Config) ->
    % Send a request with a empty Host header, expect 400 back.
    Port = ?config(hstub_port, Config),
    Req = "GET / HTTP/1.1\r\nHost:\r\n\r\n",
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket, Req),
    Data = get_until_closed(Socket, <<>>),
    M = binary:match(Data, <<"400">>),
    true = is_tuple(M),
    Config.

invalid_expect(Config) ->
    % Send invalid expect header (not 100-continue). Expect 417 back.
    Port = ?config(hstub_port, Config),
    Url = "http://localhost:" ++ integer_to_list(Port),
    {ok, {{_, 417, _}, _, _}} = httpc:request(get, {Url, [{"expect", "100-stay"}]}, [], []),
    Config.

%%%%%%%%%%%%%%%%%%%%%
%%% SETUP HELPERS %%%
%%%%%%%%%%%%%%%%%%%%%
get_until_closed(Socket, Data) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data1} ->
            get_until_closed(Socket, <<Data/binary, Data1/binary>>);
        {error, closed} ->
            Data
    end.
