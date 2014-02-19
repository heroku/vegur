-module(vegur_roundtrip_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [{group, continue}, {group, headers}, {group, http_1_0},
          {group, body_less}].

groups() -> [{continue, [], [
                back_and_forth, body_timeout, non_terminal,
                continue_upgrade_httpbis, upgrade_no_continue,
                terminal_no_continue_partial, terminal_no_continue_complete,
                no_expect_continue, http_1_0_continue,
                bypass]},
             {headers, [], [
                duplicate_different_lengths_req, duplicate_csv_lengths_req,
                duplicate_identical_lengths_req,
                duplicate_different_lengths_resp, duplicate_csv_lengths_resp,
                duplicate_identical_lengths_resp,
                delete_hop_by_hop, response_cookie_limits,
                response_header_line_limits, response_status_limits
             ]},
             {http_1_0, [], [
                advertise_1_1, conn_close_default, conn_keepalive_opt,
                chunked_to_1_0
             ]},
             {body_less, [], [
                head_small_body_expect, head_large_body_expect,
                head_no_body_expect, head_chunked_expect,
                head_close_expect,
                status_204, status_304, status_chunked_204,
                status_close_304
             ]}
            ].

%%%%%%%%%%%%
%%% Init %%%
%%%%%%%%%%%%
init_per_suite(Config) ->
    meck:new(vegur_stub, [passthrough, no_link]),
    meck:expect(vegur_stub, lookup_domain_name, fun(_, Req, HandlerState) -> {ok, test_domain, Req, HandlerState} end),
    meck:expect(vegur_stub, checkout_service, fun(_, Req, HandlerState) -> {service, test_service, Req, HandlerState} end),
    Env = application:get_all_env(vegur),
    [{vegur_env, Env} | Config].

end_per_suite(Config) ->
    [application:set_env(vegur, K, V) || {K,V} <- ?config(vegur_env, Config)],
    [vegur_stub] = meck:unload().

init_per_testcase(bypass, Config0) ->
    Config = init_per_testcase(default, Config0),
    meck:expect(vegur_stub, feature, fun(deep_continue, S) -> {disabled, S} end),
    Config;
init_per_testcase(_, Config) ->
    {ok, Listen} = gen_tcp:listen(0, [{active, false},list]),
    {ok, LPort} = inet:port(Listen),
    application:load(vegur),
    meck:expect(vegur_stub, service_backend, fun(_, Req, HandlerState) -> {{<<"127.0.0.1">>, LPort}, Req, HandlerState} end),
    {ok, ProxyPort} = application:get_env(vegur, http_listen_port),
    {ok, Started} = application:ensure_all_started(vegur),
    [{server_port, LPort},
     {proxy_port, ProxyPort},
     {server_ip, {127,0,0,1}},
     {server_listen, Listen},
     {started, Started}
     | Config].

end_per_testcase(_, Config) ->
    [application:stop(App) || App <- lists:reverse(?config(started, Config))],
    gen_tcp:close(?config(server_listen, Config)).

%%%%%%%%%%%%%%%%%%
%%% Test Cases %%%
%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%%% CONTINUE %%%
%%%%%%%%%%%%%%%%

%% 100 Continue works as you'd expect it -- send a message with a body,
%% wait for the server to send 100 Continue, then upload the body,
%% then get a response from the server.
back_and_forth(Config) ->
    %% We control both the client and the server, but not the proxy.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = req_headers(Config),
    ReqBody = req_body(),
    Resp100 = resp_100(),
    RespReal = resp(),
    Ref = make_ref(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _ReqHeaders} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp100),
    {ok, Resp100} = gen_tcp:recv(Client, 0, 1000),
    ok = gen_tcp:send(Client, ReqBody),
    {ok, ReqBody} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, RespReal),
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    {match, _} = re:run(Response, "200 OK"),
    {match, _} = re:run(Response, "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n"),
    %% Connection to the server is closed, but not to the client
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

%% In cases where the server doesn't support 100-Continue expectations,
%% the client should have a timeout duration after which it sends the body
%% anyway, and gets a valid response out of it.
body_timeout(Config) ->
    %% We control both the client and the server, but not the proxy.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Ref = make_ref(),
    ReqHeaders = req_headers(Config),
    ReqBody = req_body(),
    RespReal = resp(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    Server = get_accepted(Ref),
    %% not sending anything back, or hearing from it, send data through
    {ok, _ReqHeaders} = gen_tcp:recv(Server, 0, 1000),
    {error, timeout} = gen_tcp:recv(Client, 0, 1000),
    ok = gen_tcp:send(Client, ReqBody),
    %% longer timeout here due to how the proxy handles polling on each side
    {ok, ReqBody} = gen_tcp:recv(Server, 0, 1500),
    ok = gen_tcp:send(Server, RespReal),
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    %% everything else is fine otherwise!
    {match, _} = re:run(Response, "200 OK"),
    {match, _} = re:run(Response, "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n").

%% If a server responds to a 100-Continue expectation with two 100 Continue
%% statuses in a row, declare the request invalid.
non_terminal(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = req_headers(Config),
    ReqBody = req_body(),
    Resp100 = resp_100(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    Server = get_accepted(Ref),
    {ok, _ReqHeaders} = gen_tcp:recv(Server, 0, 1000),
    %% Send two 100 Continue in a row
    ok = gen_tcp:send(Server, Resp100),
    {ok, Resp100} = gen_tcp:recv(Client, 0, 1000),
    ok = gen_tcp:send(Client, ReqBody),
    {ok, ReqBody} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp100),
    %% Result is a 5xx status code
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    {match, _} = re:run(Response, "HTTP/1.1 5[0-9]{2} ").

%% If a request has both 100-Continue expectations and Upgrades of protocols
%% in its sleeve, HTTPbis recommends handling the 100-Continue first and
%% *then* the upgrade.
continue_upgrade_httpbis(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = upgrade_headers(Config),
    ReqBody = req_body(),
    Resp100 = resp_100(),
    Resp101 = resp_101(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    Server = get_accepted(Ref),
    {ok, _ReqHeaders} = gen_tcp:recv(Server, 0, 1000),
    %% Send the 100 Continue, then the body
    ok = gen_tcp:send(Server, Resp100),
    {ok, Resp100} = gen_tcp:recv(Client, 0, 1000),
    ok = gen_tcp:send(Client, ReqBody),
    {ok, ReqBody} = gen_tcp:recv(Server, 0, 1000),
    %% Send the 101 back
    ok = gen_tcp:send(Server, Resp101),
    %% match more broadly -- headers can be added here
    {ok, "HTTP/1.1 101 "++_} = gen_tcp:recv(Client, 0, 1000),
    %% Now we're in free roam (screw websockets har har!)
    ok = gen_tcp:send(Client, "ping"),
    {ok, "ping"} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, "pong"),
    {ok, "pong"} = gen_tcp:recv(Client, 0, 1000).

%% Allow a request with both `Upgrade` and `Expect: 100-continue` to be
%% responded to with a `101 Switching Protocol` *without* requiring the `100
%% Continue` response
upgrade_no_continue(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = upgrade_headers(Config),
    _ReqBody = req_body(), % this will get lost in time!
    Resp101 = resp_101(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    Server = get_accepted(Ref),
    {ok, _ReqHeaders} = gen_tcp:recv(Server, 0, 1000),
    %% Send the 101 back without further ado
    ok = gen_tcp:send(Server, Resp101),
    {ok, "HTTP/1.1 101 "++_} = gen_tcp:recv(Client, 0, 1000),
    %% Now we're in free roam (screw websockets har har!)
    ok = gen_tcp:send(Client, "ping"),
    {ok, "ping"} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, "pong"),
    {ok, "pong"} = gen_tcp:recv(Client, 0, 1000).

%% A terminal status code without a continue being sent should close the
%% connection to all parties if it happened during the negotiation for
%% 100 continue.
terminal_no_continue_partial(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = req_headers(Config),
    _ReqBody = req_body(),
    Resp = resp(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    Server = get_accepted(Ref),
    {ok, _ReqHeaders} = gen_tcp:recv(Server, 0, 1000),
    %% Send a terminal status right away, without having received the body
    ok = gen_tcp:send(Server, Resp),
    %% Result is a 200
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    {match, _} = re:run(Response, "200 OK"),
    {match, _} = re:run(Response, "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n"),
    {match, _} = re:run(Response, "close\r\n"),
    wait_for_closed(Server, 5000),
    wait_for_closed(Client, 5000).

%% A terminal status code without a continue being sent should ideally close
%% the connection to the server but not the client if it happened without
%% having a continue response, but after the body was entirely sent.
%%
%% HOWEVER, because the proxy cannot really know whether the request body
%% made it entirely before the server responded, we close on this one
%% anyway.
terminal_no_continue_complete(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = req_headers(Config),
    ReqBody = req_body(),
    Resp = resp(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    Server = get_accepted(Ref),
    {ok, _ReqHeaders} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Client, ReqBody),
    %% receive the body and respond
    ok = gen_tcp:send(Server, Resp),
    %% Result is a 200
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    {match, _} = re:run(Response, "200 OK"),
    {match, _} = re:run(Response, "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n"),
    {match, _} = re:run(Response, "close\r\n"),
    wait_for_closed(Server, 5000),
    wait_for_closed(Client, 5000).

%% We should forward 100 Continues sent to HTTP 1.1 clients that didn't have
%% the expect:100-continue header as per the RFC, given that older
%% HTTP 1.1 specs made it possible and current provisions mention this being
%% possible behavior to respect.
no_expect_continue(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = no_expect_headers(Config),
    ReqBody = req_body(),
    Resp100 = resp_100(),
    Resp = resp(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    ok = gen_tcp:send(Client, ReqBody),
    Server = get_accepted(Ref),
    {ok, _ReqHeaders} = gen_tcp:recv(Server, 0, 1000),
    %% receive the body and respond
    ok = gen_tcp:send(Server, Resp100),
    ok = gen_tcp:send(Server, Resp),
    %% Result is a 200, the 100 Continue should also show up. Wait a bit
    %% for all packets to come in.
    timer:sleep(100),
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    {match, _} = re:run(Response, "100 Continue"),
    {match, _} = re:run(Response, "200 OK"),
    {match, _} = re:run(Response, "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n"),
    wait_for_closed(Server, 5000).

%% We should strip 100 Continues sent to HTTP 1.0 clients that didn't have
%% the expect:100-continue header as per the RFC
http_1_0_continue(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    HTTP_1_0_Headers = http_1_0_headers(Config),
    ReqBody = req_body(),
    Resp100 = resp_100(),
    Resp = resp(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, HTTP_1_0_Headers),
    ok = gen_tcp:send(Client, ReqBody),
    Server = get_accepted(Ref),
    {ok, _ReqHeaders} = gen_tcp:recv(Server, 0, 1000),
    %% receive the body and respond
    ok = gen_tcp:send(Server, Resp100),
    ok = gen_tcp:send(Server, Resp),
    %% Result is a 200, the 100 Continue never showed up
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    {match, _} = re:run(Response, "200 OK"),
    {match, _} = re:run(Response, "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n"),
    nomatch = re:run(Response, "100 Continue"),
    wait_for_closed(Server, 5000).

%% 100 Continue can be bypassed by disabling the feature in the interface
%% module, in which case the client will see the 100 continue response,
%% but the server will not see the Expect header coming back.
%% The server may still respond with a 100 Continue the way any other HTTP/1.1
%% server is allowed, or forget about it entirely.
bypass(Config) ->
    %% We control both the client and the server, but not the proxy.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = req_headers(Config),
    ReqBody = req_body(),
    RespReal = resp(),
    Ref = make_ref(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, ReqHead} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Client, ReqBody),
    {ok, ReqBody} = gen_tcp:recv(Server, 0, 1000),
    {ok, ContResponse} = gen_tcp:recv(Client, 0, 1000),
    ok = gen_tcp:send(Server, RespReal),
    {ok, RealResponse} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    nomatch = re:run(ReqHead, "expect: ", [global,multiline,caseless]),
    {match, _} = re:run(ContResponse, "100 continue", [global,multiline,caseless]),
    {match, _} = re:run(RealResponse, "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n"),
    %% Connection to the server is closed, but not to the client
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

%%%%%%%%%%%%%%%
%%% HEADERS %%%
%%%%%%%%%%%%%%%
duplicate_different_lengths_req(Config) ->
    %% If >=2 content-length values are in the request and that they conflict,
    %% deny the request with a 400.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req =
    "POST / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 5\r\n"
    "Content-Length: 11\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n"
    "12345",
    %% Send data to the proxy right away.
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Without making it to the server, we get a 400 back. for bad request
    {ok, Resp} = gen_tcp:recv(Client, 0, 1000),
    {match, _} = re:run(Resp, "400").

duplicate_csv_lengths_req(Config) ->
    %% If >=2 content-length values are put together in a single header,
    %% deny the request with a 400.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req =
    "POST / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10,10\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n"
    "0123456789",
    %% Send data to the proxy right away.
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Without making it to the server, we get a 400 back. for bad request
    {ok, Resp} = gen_tcp:recv(Client, 0, 1000),
    {match, _} = re:run(Resp, "400").

duplicate_identical_lengths_req(Config) ->
    %% If multiple headers have similar content-length, coerce them into a
    %% single one.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req =
    "POST / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "content-length: 10\r\n"
    "content-length: 10\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n"
    "0123456789",
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    Server = get_accepted(Ref),
    %% receive the request, content-length of 10 is there once only.
    {ok, Proxied} = gen_tcp:recv(Server, 0, 1000),
    {match, [[_]]} = re:run(Proxied, "[cC]ontent-[lL]ength: 10", [{capture, all}, global]).

duplicate_different_lengths_resp(Config) ->
    %% If >=2 content-length values are in the request and that they conflict,
    %% deny the request with a 400.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp =
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 43\r\n"
    "Content-Length: 41\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n",
    Ref = make_ref(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _Req} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    {match, _} = re:run(Response, "502"),
    %% Connection to the server is closed, but not to the client
    wait_for_closed(Server, 500),
    wait_for_closed(Client, 500),
    check_stub_error({downstream, content_length}).


duplicate_csv_lengths_resp(Config) ->
    %% If >=2 content-length values are put together in a single header,
    %% deny the request with a 400.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp =
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 43,41\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n",
    Ref = make_ref(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _Req} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    {match, _} = re:run(Response, "502"),
    %% Connection to the server is closed, but not to the client
    wait_for_closed(Server, 500),
    wait_for_closed(Client, 500),
    check_stub_error({downstream, content_length}).


duplicate_identical_lengths_resp(Config) ->
    %% If multiple headers have similar content-length, coerce them into a
    %% single one.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp =
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Length: 43\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 43\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n",
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _Req} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    %% Connection to the server is closed, but not to the client
    {match, [[_]]} = re:run(Response, "[cC]ontent-[lL]ength: 43", [{capture, all}, global]).

delete_hop_by_hop(Config) ->
    %% Hop by Hop headers should be (where not passed-on on purpose) deleted
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_hops(Config),
    Resp = resp_hops(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, RecvServ} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    %% All hop by hop headers but proxy-authentication are gone
    nomatch = re:run(RecvServ, "^te:", [global,multiline,caseless]),
    nomatch = re:run(RecvServ, "^trailer:", [global,multiline,caseless]),
    nomatch = re:run(RecvServ, "^keep-alive:", [global,multiline,caseless]),
    nomatch = re:run(RecvServ, "^proxy-authorization:", [global,multiline,caseless]),
    {match,_} = re:run(RecvServ, "^proxy-authentication:", [global,multiline,caseless]),
    nomatch = re:run(RecvClient, "^te:", [global,multiline,caseless]),
    nomatch = re:run(RecvClient, "^trailer:", [global,multiline,caseless]),
    nomatch = re:run(RecvClient, "^keep-alive:", [global,multiline,caseless]),
    nomatch = re:run(RecvClient, "^proxy-authorization:", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient, "^proxy-authentication:", [global,multiline,caseless]).

response_cookie_limits(Config) ->
    %% Expect at least 8192 bytes allowed for each header line, but don't
    %% allow more for cookies, because this can be used in a DoS with shared
    %% domains. We're otherwise extremely permissive on this front.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_cookies(8193),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    {match,_} = re:run(RecvClient, "502", [global,multiline]),
    check_stub_error({downstream, cookie_length}).

response_header_line_limits(Config) ->
    %% By default a header line is restricted to 512kb when sent from
    %% the endpoint. If it goes above, a 502 is returned and the
    %% request is discarded.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_custom_headers("name", lists:duplicate(1024*1024, $a)),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    ct:pal("Rec: ~p",[RecvClient]),
    {match,_} = re:run(RecvClient, "502", [global,multiline]),
    wait_for_closed(Server, 500),
    wait_for_closed(Client, 500),
    check_stub_error({downstream, header_length}).

response_status_limits(Config) ->
    %% A status line is allowed ~8kb to parse when coming from
    %% the endpoint. If it goes above, a 502 is returned and the
    %% request is discarded.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_huge_status(10000),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    ct:pal("Rec: ~p",[RecvClient]),
    {match,_} = re:run(RecvClient, "502", [global,multiline]),
    wait_for_closed(Server, 500),
    wait_for_closed(Client, 500),
    check_stub_error({downstream, status_length}).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HTTP 1.0 BEHAVIOUR %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
advertise_1_1(Config) ->
    %% An HTTP/1.1 server should always avertise itself as such, even when
    %% talking to 1.0.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = [http_1_0_headers(Config), req_body()],
    Resp = resp_1_0(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, RecvServ} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    nomatch = re:run(RecvServ, "HTTP/1.0", [global,multiline]),
    {match,_} = re:run(RecvServ, "HTTP/1.1", [global,multiline]),
    nomatch = re:run(RecvClient, "HTTP/1.0", [global,multiline]),
    {match,_} = re:run(RecvClient, "HTTP/1.1", [global,multiline]).

conn_close_default(Config) ->
    %% By default, all connections are closed after each request
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = [http_1_0_headers(Config), req_body()],
    Resp = resp(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, _} = gen_tcp:recv(Client, 0, 1000),
    %% Check final connection status
    wait_for_closed(Server, 500),
    wait_for_closed(Client, 500).

conn_keepalive_opt(Config) ->
    %% By default, all connections are closed after each request
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = [http_1_0_keepalive_headers(Config), req_body()],
    Resp = resp(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Check final connection status
    {match,_} = re:run(RecvClient, "^connection: ?keep-alive", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

chunked_to_1_0(Config) ->
    %% Chunked content from a 1.1 server to a 1.0 client unchunks the content
    %% and streams it as a request delimited by the end of the connection.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = [http_1_0_headers(Config), req_body()],
    Resp = [resp_headers(chunked), "3\r\nabc\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuv\r\n"
                                   "0\r\n\r\n"],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    Response = recv_until_close(Client),
    nomatch = re:run(Response, "chunked", [global,multiline,caseless]),
    {match,_} = re:run(Response, "connection: ?close", [global,multiline,caseless]),
    {match,_} = re:run(Response, "abcdefghijklmnopqrstu", [global,multiline,caseless]),
    %% Check final connection status
    wait_for_closed(Server, 500).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BODY LESS REQUEST BEHAVIOUR %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
head_small_body_expect(Config) ->
    %% A HEAD request should have the proxy be able to proceed without
    %% waiting for a body even with headers indicating one otherwise.
    %% To test it, we're gonna see if the entire stack blocks waiting
    %% for a body by doing a request<-->response. If one of
    %% them gets blocked, the proxy is stuck waiting on a body for a
    %% HEAD request's response, and the connection to the server
    %% won't close.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_head(Config),
    Resp = resp_headers(43),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^content-length: 43", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

head_large_body_expect(Config) ->
    %% Same as above, but for larger bodies getting streamed
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_head(Config),
    Resp = resp_headers(430000),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^content-length: 430000", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

head_no_body_expect(Config) ->
    %% Same as above, but for larger bodies getting streamed
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_head(Config),
    Resp = resp_headers(0),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^content-length: 0", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

head_chunked_expect(Config) ->
    %% Same as above, but for larger bodies getting streamed
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_head(Config),
    Resp = resp_headers(chunked),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^transfer-encoding: chunked", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

head_close_expect(Config) ->
    %% Same as above, but for larger bodies getting streamed
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_head(Config),
    Resp = resp_headers(close),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^connection: close", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

status_204(Config) ->
    %% For a 204 response, we should not wait for a request body. We will not,
    %% however, strip any headers that are not hop by hop.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_204(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^content-length:", [global,multiline,caseless]),
    nomatch = re:run(Recv, "body", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

status_304(Config) ->
    %% For a 304 response, we should not wait for a request body. We will not,
    %% however, strip any headers that are not hop by hop.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_304(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^content-length:", [global,multiline,caseless]),
    nomatch = re:run(Recv, "body", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

status_chunked_204(Config) ->
    %% For a 204 response, we should not wait for a request body. We will not,
    %% however, strip any headers that are not hop by hop.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_204(chunked),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "chunked", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

status_close_304(Config) ->
    %% For a 304 response, we should not wait for a request body. We will not,
    %% however, strip any headers that are not hop by hop.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_304(close),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^connection: close", [global,multiline,caseless]),
    nomatch = re:run(Recv, "body", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
start_acceptor(Ref, Config) ->
    Listen = ?config(server_listen, Config),
    Parent = self(),
    spawn_link(fun() ->
        {ok, Accept} = gen_tcp:accept(Listen, 30000),
        ok = gen_tcp:controlling_process(Accept, Parent),
        Parent ! {Ref, Accept}
    end),
    ok.

get_accepted(Ref) ->
    receive
        {Ref, Accepted} -> Accepted
    after 5000 ->
        error(too_long)
    end.

recv_until_close(Port) ->
    case gen_tcp:recv(Port, 0, 100) of
        {error, closed} -> [];
        {ok, Data} -> [Data | recv_until_close(Port)]
    end.

wait_for_closed(_Port, T) when T =< 0 -> error(not_closed);
wait_for_closed(Port, T) ->
    case gen_tcp:recv(Port, 0, 0) of
        {error, closed} ->
            ok;
        _ ->
            timer:sleep(100),
            wait_for_closed(Port, T-100)
    end.

%% Request data
req_headers(Config) ->
    "POST /continue-1 HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10\r\n"
    "Expect: 100-continue\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n".

upgrade_headers(Config) ->
    "GET /continue-1 HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10\r\n"
    "Expect: 100-continue\r\n"
    "Content-Type: text/plain\r\n"
    "Sec-WebSocket-Key: HKmTu9J7PdPcys6iEYNH7g==\r\n"
    "Upgrade: websocket\r\n"
    "Connection: Upgrade\r\n"
    "Sec-WebSocket-Version: 13\r\n"
    "\r\n".

http_1_0_headers(Config) ->
    "POST /continue-1 HTTP/1.0\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n".

http_1_0_keepalive_headers(Config) ->
    "POST /continue-1 HTTP/1.0\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10\r\n"
    "Content-Type: text/plain\r\n"
    "Connection: keep-alive\r\n"
    "\r\n".

no_expect_headers(Config) ->
    "POST /continue-1 HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n".

req_body() ->
    "0123456789".

resp_100() ->
    "HTTP/1.1 100 Continue\r\n\r\n".

resp_101() ->
    "HTTP/1.1 101 Switching Protocols\r\n\r\n".

req(Config) ->
    "POST / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 5\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n"
    "12345".

req_head(Config) ->
    "HEAD / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 5\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n"
    "12345".

req_hops(Config) ->
    "POST / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 5\r\n"
    "Content-Type: text/plain\r\n"
    %% some hop by hop headers
    "Connection: keep-alive\r\n"
    "TE: deflate\r\n"
    "TE: trailers\r\n"
    "Trailer: some-header\r\n"
    "keep-alive: timeout=213\r\n"
    "prOxy-Authorization: whatever\r\n"
    "proxy-AuthentiCation: 0\r\n"
    "\r\n"
    "12345".

resp() ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 43\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n".

resp_headers(chunked) ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Transfer-Encoding: chunked\r\n"
    "\r\n";
resp_headers(close) ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "connection: close\r\n"
    "\r\n";
resp_headers(Size) when is_integer(Size) ->
    resp_headers(integer_to_list(Size));
resp_headers(Size) when is_list(Size) ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: "++Size++"\r\n"
    "\r\n".

resp_hops() ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 43\r\n"
    %% some hop by hop headers
    "Connection: keep-alive\r\n"
    "TE: deflate\r\n"
    "TE: trailers\r\n"
    "Trailer: some-header\r\n"
    "keep-alive: timeout=213\r\n"
    "prOxy-Authorization: whatever\r\n"
    "proxy-AuthentiCation: 0\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n".

resp_cookies(Size) ->
    Val = lists:duplicate(max(1,Size-6), $a),
    resp_custom_headers("Set-Cookie", "name="++Val++";").

resp_custom_headers(Name, Val) ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    ++Name++": "++Val++"\r\n"
    "Content-Length: 43\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n".

resp_huge_status(Len) ->
    Status = lists:duplicate(Len-6, $X),
    "HTTP/1.1 "++Status++"\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 43\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n".

resp_204(chunked) ->
    "HTTP/1.1 204 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "transfer-encoding: chuNked\r\n"
    "\r\n".

resp_204() ->
    "HTTP/1.1 204 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 14\r\n"
    "\r\n"
    "misplaced body".

resp_304(close) ->
    "HTTP/1.1 304 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "connection:close\r\n"
    "\r\n"
    "misplaced body with a bigger but wrong size".

resp_304() ->
    "HTTP/1.1 304 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 1421093\r\n"
    "\r\n"
    "misplaced body with a bigger but wrong size".

resp_1_0() ->
    "HTTP/1.0 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 43\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n".

domain(Config) ->
    LPort = ?config(server_port, Config),
    binary_to_list(<<"127.0.0.1:", (integer_to_binary(LPort))/binary>>).

check_stub_error(Pattern) ->
    Local = [Args || {_, {vegur_stub, error_page, Args}, _Ret} <- meck:history(vegur_stub)],
    ct:pal("Local: ~p~n", [Local]),
    Pattern = hd(lists:last(Local)).
