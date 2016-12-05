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
-module(vegur_request_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, vegur_request_handling}
     ,{group, vegur_request_mocks}
     ,{group, vegur_request_upgrade}
     ,{group, vegur_request_lookups}
    ].

groups() ->
    [
     {vegur_request_handling, [], [no_host
                                   ,empty_host
                                   ,empty_expect
                                   ,url_limits
                                   ,header_line_limits
                                   ,header_count_limits
                                   ,invalid_expect
                                   ,absolute_uri
                                  ]},
     {vegur_request_mocks, [], [herokuapp_redirect
                                ,maintainance_mode_on
                               ]},
     {vegur_request_upgrade, [], [upgrade_invalid
                                  ,upgrade_websockets
                                 ]},
     {vegur_request_lookups, [], [no_route
                                  ,backlog_timeout
                                  ,backlog_too_deep
                                  ,conn_limit_reached
                                  ,route_lookup_failed
                                  ,no_web_processes
                                  ,crashed
                                  ,backends_quarantined
                                  ,backends_starting
                                  ,backends_idle
                                  ,app_blank
                                  ,app_not_found
                                  ,app_lookup_failed
                                 ]}
    ].

init_per_suite(Config) ->
    application:load(vegur),
    {ok, Cowboyku} = application:ensure_all_started(cowboyku),
    {ok, Inets} = application:ensure_all_started(inets),
    VegurPort = 9333,
    {ok, _} = vegur:start_http(VegurPort, vegur_stub, []),
    [{started, Cowboyku++Inets},
     {vegur_port, VegurPort} | Config].

end_per_suite(Config) ->
    meck:unload(),
    vegur:stop_http(),
    application:unload(vegur),
    [application:stop(App) || App <- lists:reverse(?config(started, Config))],
    ok.

init_per_group(vegur_request_handling, Config) ->
    Config;
init_per_group(vegur_request_mocks, Config) ->
    {ok, MeckStarted} = application:ensure_all_started(meck),
    [{meck_started, MeckStarted} | Config];
init_per_group(vegur_request_upgrade, Config) ->
    TestDomain = <<"vegurtest.testdomain">>,
    ok = mock_middlewares([vegur_upgrade_middleware, vegur_test_middleware]),
    [{test_domain, TestDomain} | Config];
init_per_group(vegur_request_lookups, Config) ->
    {ok, MeckStarted} = application:ensure_all_started(meck),
    TestDomain = <<"vegurtest.testdomain">>,
    meck:expect(vegur_stub, lookup_domain_name,
                fun(Domain, Req, HandlerState) ->
                        Domain = TestDomain,
                        {ok, mocked_domain_group, Req, HandlerState}
                end),
    [{meck_started, MeckStarted},
     {test_domain, TestDomain} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(vegur_request_mocks, Config) ->
    meck:unload(),
    [application:stop(App)
     || App <- lists:reverse(?config(meck_started, Config))],
    Config;
end_per_group(vegur_request_upgrade, Config) ->
    ok = unmock_middlewares(),
    Config;
end_per_group(vegur_request_lookups, Config) ->
    [meck:unload(Mod) || Mod <- [vegur_stub]],
    [application:stop(App) || App <- lists:reverse(?config(meck_started, Config))],
    Config;
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(no_host, Config) ->
    Config;
init_per_testcase(empty_host, Config) ->
    Config;
init_per_testcase(url_limits, Config) ->
    meck:new(vegur_stub, [passthrough]),
    meck:expect(vegur_stub, checkout_service,
                fun(_DomainGroup, Req, State) -> {error, please_die, Req, State} end),
    Config;
init_per_testcase(header_line_limits, Config) ->
    meck:new(vegur_stub, [passthrough]),
    meck:expect(vegur_stub, checkout_service,
                fun(_DomainGroup, Req, State) -> {error, please_die, Req, State} end),
    Config;
init_per_testcase(header_count_limits, Config) ->
    meck:new(vegur_stub, [passthrough]),
    meck:expect(vegur_stub, checkout_service,
                fun(_DomainGroup, Req, State) -> {error, please_die, Req, State} end),
    Config;
init_per_testcase(invalid_expect, Config) ->
    Config;
init_per_testcase(herokuapp_redirect, Config) ->
    meck:expect(vegur_stub, lookup_domain_name,
                fun(Domain, Req, HandlerState) ->
                        RootDomainToReplace = <<"oldstub">>,
                        RootDomainToReplaceWith = <<"vegur">>,
                        NewDomain = re:replace(Domain, RootDomainToReplace, RootDomainToReplaceWith),
                        {redirect, herokuapp_redirect, [], NewDomain, Req, HandlerState}
                end),
    HerokuDomain = <<"oldstub">>,
    TestDomain = <<"vegurtest.", HerokuDomain/binary>>,
    [{test_domain, TestDomain} | Config];
init_per_testcase(maintainance_mode_on, Config) ->
    TestDomain = <<"vegurtest.testdomain">>,
    meck:expect(vegur_stub, lookup_domain_name,
                fun(Domain, Req, HandlerState) ->
                        Domain = TestDomain,
                        {ok, mocked_domain_group, Req, HandlerState}
                end),
    ok = mock_service_reply(error, maintainance_mode),
    [{test_domain, TestDomain} | Config];
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(herokuapp_redirect, Config) ->
    meck:unload(),
    Config;
end_per_testcase(maintainance_mode_on, Config) ->
    meck:unload(),
    Config;
end_per_testcase(url_limits, Config) ->
    meck:unload(),
    Config;
end_per_testcase(header_line_limits, Config) ->
    meck:unload(),
    Config;
end_per_testcase(header_count_limits, Config) ->
    meck:unload(),
    Config;
end_per_testcase(_, Config) ->
    Config.

%%%===================================================================
%%% Test cases
%%%===================================================================
no_host(Config) ->
    % Send a request with no Host header, expect 400 back. HTTPc doesn't allow me to do
    % invalid requests, so this one manual.
    Port = ?config(vegur_port, Config),
    Req = "GET / HTTP/1.1\r\n\r\n",
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket, Req),
    Data = get_until_closed(Socket, <<>>),
    M = binary:match(Data, <<"400">>),
    true = is_tuple(M),
    Config.

empty_host(Config) ->
    % Send a request with a empty Host header, expect 400 back.
    Port = ?config(vegur_port, Config),
    Req = "GET / HTTP/1.1\r\nHost:\r\n\r\n",
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket, Req),
    Data = get_until_closed(Socket, <<>>),
    M = binary:match(Data, <<"400">>),
    true = is_tuple(M),
    Config.

empty_expect(Config) ->
    % Send a request with a empty Expect header, expect anything but 400 or 417 back.
    Port = ?config(vegur_port, Config),
    Req = "GET / HTTP/1.1\r\nHost:localhost\r\nExpect:\r\n\r\n",
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket, Req),
    Data = get_until_closed(Socket, <<>>),
    nomatch = binary:match(Data, <<"400">>),
    nomatch = binary:match(Data, <<"417">>),
    Config.

url_limits(Config) ->
    % Expect at least 8192 bytes allowed for URLs in request lines. Anything
    % over this fails with a 414: Request-URI Too Long
    Port = ?config(vegur_port, Config),
    Req1 = "GET /"++lists:duplicate(10000, $a)++" HTTP/1.1\r\nHost:localhost\r\n\r\n",
    {ok, Socket1} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket1, Req1),
    Data1 = get_until_closed(Socket1, <<>>),
    {_, _} = binary:match(Data1, <<"414">>),
    %% Accepted
    Req2 = "GET /"++lists:duplicate(8200, $a)++" HTTP/1.1\r\nHost:localhost\r\nConnection:close\r\n\r\n",
    {ok, Socket2} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket2, Req2),
    Data2 = get_until_closed(Socket2, <<>>),
    nomatch = binary:match(Data2, <<"414">>),
    Config.

header_line_limits(Config) ->
    % Expect at least 8192 bytes allowed for each header line, and 1000 characters
    % for header names.
    %% Long header names
    Port = ?config(vegur_port, Config),
    Req1 = "GET / HTTP/1.1\r\nHost:localhost\r\n"
           ++lists:duplicate(2100, $a)++": ok\r\n\r\n",
    {ok, Socket1} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket1, Req1),
    Data1 = get_until_closed(Socket1, <<>>),
    {_, _} = binary:match(Data1, <<"400">>),
    %% Works
    Req2 = "GET / HTTP/1.1\r\nHost:localhost\r\nconnection: close\r\n"
           ++lists:duplicate(1000, $a)++": ok\r\n\r\n",
    {ok, Socket2} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket2, Req2),
    Data2 = get_until_closed(Socket2, <<>>),
    nomatch = binary:match(Data2, <<"400">>),
    %% Long headers total values
    Req3 = "GET / HTTP/1.1\r\nHost:localhost\r\n"
           "h: "++lists:duplicate(9100, $a)++"\r\n\r\n",
    {ok, Socket3} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket3, Req3),
    Data3 = get_until_closed(Socket3, <<>>),
    {_, _} = binary:match(Data3, <<"400">>),
    %% Works
    Req4 = "GET / HTTP/1.1\r\nHost:localhost\r\nconnection:close\r\n"
           "h: "++lists:duplicate(8192, $a)++"\r\n\r\n",
    {ok, Socket4} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket4, Req4),
    Data4 = get_until_closed(Socket4, <<>>),
    nomatch = binary:match(Data4, <<"400">>),
    Config.

header_count_limits(Config) ->
    % Expect at least 1000 headers total, without regards to size
    % over this fails with a 400
    Port = ?config(vegur_port, Config),
    Req1 = ["GET / HTTP/1.1\r\nHost:localhost\r\n",
            ["header"++integer_to_list(N)++":1\r\n" || N <- lists:seq(1,1100)],
            "\r\n"],
    {ok, Socket1} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket1, Req1),
    Data1 = get_until_closed(Socket1, <<>>),
    cthr:pal("Data1: ~p", [Data1]),
    {_, _} = binary:match(Data1, <<"400">>),
    %% Accepted
    Req2 = ["GET / HTTP/1.1\r\nHost:localhost\r\nconnection:close\r\n",
            ["header"++integer_to_list(N)++":1\r\n" || N <- lists:seq(1,999)], % 999+host
            "\r\n"],
    {ok, Socket2} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket2, Req2),
    Data2 = get_until_closed(Socket2, <<>>),
    cthr:pal("Data2: ~p", [Data2]),
    nomatch = binary:match(Data2, <<"400">>),
    Config.

invalid_expect(Config) ->
    % Send invalid expect header (not 100-continue). Expect 417 back.
    Port = ?config(vegur_port, Config),
    Url = "http://localhost:" ++ integer_to_list(Port),
    {ok, {{_, 417, _}, _, _}} = httpc:request(get, {Url, [{"expect", "100-stay"}]}, [], []),
    Config.

absolute_uri(Config) ->
    % Make a request with a absolute URI. This is a valid request (used for CONNECT among other things
    % but it is not supported by us, we expect a 400 back. This is handled by Cowboyku.
    Port = ?config(vegur_port, Config),
    Req = "CONNECT http://example.com:80 HTTP/1.1\r\nHost: localhost\r\n\r\n",
    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [{active,false}, binary]),
    ok = gen_tcp:send(Socket, Req),
    Data = get_until_closed(Socket, <<>>),
    M = binary:match(Data, <<"400">>),
    true = is_tuple(M),
    Config.

herokuapp_redirect(Config) ->
    % Get the 'automatic' redirect from old heroku domains (foo.HEROKU_DOMAIN) to new heroku domains
    % (foo.HEROKUAPP_DOMAIN).
    Port = ?config(vegur_port, Config),
    Domain = ?config(test_domain, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/query_line?foo=bar",
    {ok, {{_, 301, "Moved Permanently"}, Headers, _}} = httpc:request(get, {Url, [{"host", binary_to_list(Domain)}]}, [{autoredirect, false}], []),
    "http://vegurtest.vegur/query_line?foo=bar" = proplists:get_value("location", Headers),
    Config.

maintainance_mode_on(Config) ->
    % Request a fake domain that's configured in maintainance mode.
    Port = ?config(vegur_port, Config),
    Domain = ?config(test_domain, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 503, _}, _, _}} = httpc:request(get, {Url, [{"host", binary_to_list(Domain)}]}, [], []),
    Config.

upgrade_invalid(Config) ->
    Port = ?config(vegur_port, Config),
    Domain = ?config(test_domain, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 400, _}, _, _}} = httpc:request(get, {Url, [{"host", binary_to_list(Domain)},
                                                          {"connection", "upgrade"}]}, [], []),
    Config.

upgrade_websockets(Config) ->
    Port = ?config(vegur_port, Config),
    Domain = ?config(test_domain, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, X, _}, _, _}} = httpc:request(get, {Url, [{"host", binary_to_list(Domain)},
                                                        {"upgrade", "WebSocket"},
                                                        {"connection", "Upgrade Keep-Alive"},
                                                        {"sec-websocket-key", "dGhlIHNhbXBsZSBub25jZQ=="}
                                                       ]}, [], []),
    cthr:pal("Request over"),
    true = X < 300,
    Config.

no_route(Config) ->
    ok = mock_service_reply(error, no_route_id),
    {ok, {{_, 502, _}, _, _}} = service_request(Config),
    Config.

backlog_timeout(Config) ->
    ok = mock_service_reply(error, {backlog_timeout, 100, 100}),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
    Config.

backlog_too_deep(Config) ->
    ok = mock_service_reply(error, {backlog_too_deep, 100, 100}),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
    Config.

conn_limit_reached(Config) ->
    ok = mock_service_reply(error, {conn_limit_reached, 100, 100}),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
    Config.

route_lookup_failed(Config) ->
    ok = mock_service_reply(error, route_lookup_failed),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
    Config.

no_web_processes(Config) ->
    ok = mock_service_reply(error, no_web_processes),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
    Config.

crashed(Config) ->
    ok = mock_service_reply(error, crashed),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
    Config.

backends_quarantined(Config) ->
    ok = mock_service_reply(error, backends_quarantined),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
    Config.

backends_starting(Config) ->
    ok = mock_service_reply(error, backends_starting),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
    Config.

backends_idle(Config) ->
    ok = mock_service_reply(error, backends_idle),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
    Config.

app_blank(Config) ->
    ok = mock_service_reply(error, app_blank),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
    Config.

app_not_found(Config) ->
    ok = mock_service_reply(error, app_not_found),
    {ok, {{_, 404, _}, _, _}} = service_request(Config),
    Config.

app_lookup_failed(Config) ->
    ok = mock_service_reply(error, app_lookup_failed),
    {ok, {{_, 503, _}, _, _}} = service_request(Config),
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

service_request(Config) ->
    Port = ?config(vegur_port, Config),
    Domain = ?config(test_domain, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    httpc:request(get, {Url, [{"host", binary_to_list(Domain)}]}, [], []).

mock_service_reply(error, Reason) ->
    meck:expect(vegur_stub, checkout_service,
                fun(_, Req, H) ->
                        {error, Reason, Req, H}
                end).

mock_middlewares(Middlewares) ->
    meck:new(vegur_utils, [no_link, passthrough]),
    meck:expect(vegur_utils, config, fun(middleware_stack) ->
                                             Middlewares;
                                        (Other) ->
                                             meck:passthrough([Other])
                                     end),
    ok.

unmock_middlewares() ->
    meck:unload(vegur_utils).

set_middlewares(RanchRef, Middlewares) ->
    OldOpts = ranch:get_protocol_options(RanchRef),
    OldMiddlewares = proplists:get_value(middlewares, OldOpts),
    Opts2 = lists:keyreplace(middlewares, 1, OldOpts, {middlewares, Middlewares}),
    ok = ranch:set_protocol_options(RanchRef, Opts2),
    {ok, OldMiddlewares}.
