-module(hstub_continue_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [back_and_forth, body_timeout, non_terminal, continue_upgrade_httpbis,
          upgrade_no_continue, terminal_no_continue_partial,
          terminal_no_continue_complete, no_expect_continue, http_1_0_continue].

%%%%%%%%%%%%
%%% Init %%%
%%%%%%%%%%%%
init_per_suite(Config) ->
    meck:new(hstub_stub, [passthrough, no_link]),
    meck:expect(hstub_stub, in_maintenance_mode, fun(_) -> false end),
    meck:expect(hstub_stub, lookup_domain_name, fun(_) -> {ok, test_domain} end),
    meck:expect(hstub_stub, lookup_service, fun(_) -> {route, test_service} end),
    Env = application:get_all_env(hstub),
    [{hstub_env, Env} | Config].

end_per_suite(Config) ->
    [application:set_env(hstub, K, V) || {K,V} <- ?config(hstub_env, Config)],
    [hstub_stub] = meck:unload().

init_per_testcase(_, Config) ->
    {ok, Listen} = gen_tcp:listen(0, [{active, false},list]),
    {ok, LPort} = inet:port(Listen),
    application:load(hstub),
    %ok = application:set_env(hstub, domain, <<"127.0.0.1">>),
    meck:expect(hstub_stub, service_backend, fun(_) -> {<<"127.0.0.1">>, LPort} end),
    %ok = application:set_env(hstub, backend, {<<"127.0.0.1">>, LPort}),
    {ok, ProxyPort} = application:get_env(hstub, http_listen_port),
    {ok, Started} = application:ensure_all_started(hstub),
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
    ct:pal("Response: ~p",[Response]),
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
    ct:pal("Response: ~p",[Response]),
    {match, _} = re:run(Response, "200 OK"),
    {match, _} = re:run(Response, "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n"),
    nomatch = re:run(Response, "100 Continue"),
    wait_for_closed(Server, 5000).

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
    LPort = ?config(server_port, Config),
    Domain = <<"127.0.0.1:", (integer_to_binary(LPort))/binary>>,
    "POST /continue-1 HTTP/1.1\r\n"
    "Host: "++binary_to_list(Domain)++"\r\n"
    "Content-Length: 10\r\n"
    "Expect: 100-continue\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n".

upgrade_headers(Config) ->
    LPort = ?config(server_port, Config),
    Domain = <<"127.0.0.1:", (integer_to_binary(LPort))/binary>>,
    "GET /continue-1 HTTP/1.1\r\n"
    "Host: "++binary_to_list(Domain)++"\r\n"
    "Content-Length: 10\r\n"
    "Expect: 100-continue\r\n"
    "Content-Type: text/plain\r\n"
    "Sec-WebSocket-Key: HKmTu9J7PdPcys6iEYNH7g==\r\n"
    "Upgrade: websocket\r\n"
    "Connection: Upgrade\r\n"
    "Sec-WebSocket-Version: 13\r\n"
    "\r\n".

http_1_0_headers(Config) ->
    LPort = ?config(server_port, Config),
    Domain = <<"127.0.0.1:", (integer_to_binary(LPort))/binary>>,
    "POST /continue-1 HTTP/1.0\r\n"
    "Host: "++binary_to_list(Domain)++"\r\n"
    "Content-Length: 10\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n".

no_expect_headers(Config) ->
    LPort = ?config(server_port, Config),
    Domain = <<"127.0.0.1:", (integer_to_binary(LPort))/binary>>,
    "POST /continue-1 HTTP/1.1\r\n"
    "Host: "++binary_to_list(Domain)++"\r\n"
    "Content-Length: 10\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n".

req_body() ->
    "0123456789".

resp_100() ->
    "HTTP/1.1 100 Continue\r\n\r\n".

resp_101() ->
    "HTTP/1.1 101 Switching Protocols\r\n\r\n".

resp() ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 43\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n".
