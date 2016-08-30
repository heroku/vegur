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
-module(vegur_roundtrip_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [{group, continue}, {group, headers}, {group, http_1_0},
          {group, chunked}, {group, body_less}, {group, large_body},
          {group, backend_close}, {group, backend_keepalive}].

groups() -> [{continue, [], [
                back_and_forth, body_timeout, non_terminal,
                continue_upgrade_httpbis, upgrade_no_continue,
                terminal_no_continue_partial, terminal_no_continue_complete,
                no_expect_continue, http_1_0_continue,
                unknown_expect, bad_syntax_expect, bypass]},
             {headers, [], [
                duplicate_different_lengths_req, duplicate_csv_lengths_req,
                duplicate_identical_lengths_req,
                duplicate_different_lengths_resp, duplicate_csv_lengths_resp,
                duplicate_identical_lengths_resp,
                delete_hop_by_hop, response_cookie_limits,
                response_header_line_limits, response_status_limits,
                via, via_chain, bad_status, preserve_case, header_order,
                host_nofold, host_conflict
             ]},
             {http_1_0, [], [
                advertise_1_1, conn_close_default, conn_keepalive_opt,
                chunked_to_1_0
             ]},
             {chunked, [], [
                passthrough, passthrough_short_crlf, passthrough_early_0length,
                passthrough_partial_early_0length1, passthrough_partial_early_0length2,
                passthrough_partial_early_0length3,
                interrupted_client, interrupted_server, bad_chunk, trailers,
                trailers_close, trailers_client_err, trailers_serv_err,
                trailers_no_header
             ]},
             {body_less, [], [
                head_small_body_expect, head_large_body_expect,
                head_no_body_expect, head_chunked_expect,
                head_close_expect, head_close_expect_client,
                status_204, status_304, status_chunked_204,
                status_close_304, status_close_304_client,
                bad_transfer_encoding, valid_status_204, valid_status_304,
                valid_head
             ]},
             {large_body, [], [
                large_body_stream, large_body_close, large_body_close_delimited,
                large_body_request_response_interrupt,
                large_chunked_request_response_interrupt,
                large_close_request_response_interrupt,
                large_close_request_response_interrupt_undetected,
                weird_content_length
             ]},
             {backend_close, [], [
                %% test names go <option>_<client-conn-header>-<backend-conn-header>[_<suffix>]
                close_close_close, close_keepalive_keepalive, close_default_default,
                close_close_keepalive, close_keepalive_close
             ]},
             {backend_keepalive, [], [
                %% test names go <option>_<client-conn-header>-<backend-conn-header>[_<suffix>]
                keepalive_close_close, keepalive_keepalive_keepalive,
                keepalive_default_default, keepalive_close_keepalive,
                keepalive_keepalive_close,
                %% different tests, assuming keepalive is end to end
                keepalive_reset,
                keepalive_http_1_0, keepalive_upgrade_close_server,
                keepalive_upgrade_close_client,
                keepalive_chunked, keepalive_close_delimited,
                keepalive_large_body
             ]}
            ].

%%%%%%%%%%%%
%%% Init %%%
%%%%%%%%%%%%
init_per_suite(Config) ->
    IP = {A,B,C,D} = pick_interface(),
    TxtIP = iolist_to_binary([integer_to_list(A), ".",
                              integer_to_list(B), ".",
                              integer_to_list(C), ".",
                              integer_to_list(D)]),
    {ok, Port} = gen_tcp:listen(0, [{ip,IP}]),
    {ok, [{recbuf, RecBuf}]} = inet:getopts(Port, [recbuf]),
    ct:pal("BUF DEFAULT: ~p~n",[inet:getopts(Port, [buffer, recbuf, packet_size])]),
    application:load(vegur),
    meck:new(vegur_stub, [passthrough, no_link]),
    meck:expect(vegur_stub, lookup_domain_name, fun(_, Req, HandlerState) -> {ok, test_domain, Req, HandlerState} end),
    meck:expect(vegur_stub, checkout_service, fun(_, Req, HandlerState) -> {service, test_service, Req, HandlerState} end),
    Env = application:get_all_env(vegur),
    {ok, Started} = application:ensure_all_started(vegur),
    [{vegur_env, Env},
     {ip, IP}, {txt_ip, TxtIP},
     {default_tcp_recbuf, RecBuf},
     {started, Started} | Config].

end_per_suite(Config) ->
    [application:stop(App) || App <- lists:reverse(?config(started, Config))],
    [application:set_env(vegur, K, V) || {K,V} <- ?config(vegur_env, Config)],
    [vegur_stub] = meck:unload(),
    application:unload(vegur),
    Config.

init_per_group(backend_keepalive, Config) ->
    [{keepalive, true} | Config];
init_per_group(_, Config) ->
    [{keepalive, false} | Config].

end_per_group(backend_keepalive, Config) ->
    Config;
end_per_group(_, Config) ->
    Config.

init_per_testcase(bypass, Config0) ->
    Config = init_per_testcase(default, Config0),
    meck:expect(vegur_stub, feature, fun(deep_continue, S) -> {disabled, S}; (_, S) -> {disabled, S} end),
    Config;
init_per_testcase(response_header_line_limits, Config0) ->
    Config = init_per_testcase({catchall, make_ref()}, Config0),
    Default = vegur_utils:config(client_tcp_buffer_limit),
    application:set_env(vegur, client_tcp_buffer_limit, ?config(default_tcp_recbuf, Config)),
    [{default_tcp, Default} | Config];
init_per_testcase(response_status_limits, Config0) ->
    Config = init_per_testcase({catchall, make_ref()}, Config0),
    Default = vegur_utils:config(client_tcp_buffer_limit),
    application:set_env(vegur, client_tcp_buffer_limit, ?config(default_tcp_recbuf, Config)),
    [{default_tcp, Default} | Config];
init_per_testcase(nohost_1_0, _Config) ->
    {skip, "Host header required for HTTP/1.0 even with an absolute path."};
init_per_testcase(keepalive_reset, Config) ->
    init_per_testcase(just_fall_through_for_default,
                      [{keepalive,twice}|Config]);
init_per_testcase(_, Config) ->
    IP = ?config(ip, Config),
    TxtIP = ?config(txt_ip, Config),
    {ok, Listen} = gen_tcp:listen(0, [{active, false},list, {ip, IP}]),
    {ok, LPort} = inet:port(Listen),
    meck:expect(vegur_stub, service_backend,
                fun(_, Req, HandlerState) ->
                        case ?config(keepalive, Config) of
                            true ->
                                {{keepalive, {default, {TxtIP, LPort}}}, Req, HandlerState};
                            twice ->
                                % oh this is hacky. We want keepalive only twice every time!
                                case get(mock_keepalive) of
                                    undefined ->
                                        put(mock_keepalive, 1),
                                        {{keepalive, {default, {TxtIP, LPort}}}, Req, HandlerState};
                                    1 ->
                                        put(mock_keepalive, 0),
                                        {{keepalive, {default, {TxtIP, LPort}}}, Req, HandlerState};
                                    0 ->
                                        put(mock_keepalive, 1),
                                        {{keepalive, {new, {TxtIP, LPort}}}, Req, HandlerState}
                                end;
                            _ ->
                                {{TxtIP, LPort}, Req, HandlerState}
                        end
                end),
    {ok, _} = vegur:start_http(9880, vegur_stub, []),
    [{server_port, LPort},
     {proxy_port, 9880},
     {server_ip, IP},
     {server_listen, Listen}
     | Config].

end_per_testcase(response_header_line_limits, Config) ->
    Default = ?config(default_tcp, Config),
    application:set_env(vegur, client_tcp_buffer_limit, Default),
    end_per_testcase({catchall, make_ref()}, Config);
end_per_testcase(response_status_limits, Config) ->
    Default = ?config(default_tcp, Config),
    application:set_env(vegur, client_tcp_buffer_limit, Default),
    end_per_testcase({catchall, make_ref()}, Config);
end_per_testcase(_, Config) ->
    vegur:stop_http(),
    gen_tcp:close(?config(server_listen, Config)).

pick_interface() ->
    %% If possible, return a non-loopback interface so that we can get
    %% more realistic testing scenarios. The loopback interface has all
    %% kinds of OS optimizations around connection termination and error
    %% detection that can make the tests non-representative of the real world.
    {ok, Interfaces} = inet:getifaddrs(),
    MixedInterfaces = lists:flatten([Attrs || {_Name, Attrs} <- Interfaces]),
    IPv4s = [IPv4 || {addr, IPv4={_,_,_,_}} <- MixedInterfaces,
                     IPv4 =/= {127,0,0,1}],
    case IPv4s of
        [] -> % Fallback
            ct:pal("Falling back to loopback interface"),
            {127,0,0,1};
        [H|_] ->
            ct:pal("Using IP ~p for tests", [H]),
            H
    end.

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

%% When an unknown expect header value is used, we should fail with a 417
%% error.
unknown_expect(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = custom_expect_headers(Config, "whatever"),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    %% No data to exchange because this failed
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    {match, _} = re:run(Response, "417").

%% When an unknown expect header value is used, we should fail with a 417
%% error, even if the header is unparseable by the standards
bad_syntax_expect(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = custom_expect_headers(Config, "100 continue"),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    %% No data to exchange because this failed
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    {match, _} = re:run(Response, "400").

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
    %% Connection to the server is closed, also to the client
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
    %% All hop by hop headers but proxy-authentication and trailers are gone
    ct:pal("SRV:~p~n",[RecvServ]),
    ct:pal("CLI:~p~n",[RecvClient]),
    nomatch = re:run(RecvServ, "^te:", [global,multiline,caseless]),
    {match,_} = re:run(RecvServ, "^trailer:", [global,multiline,caseless]),
    nomatch = re:run(RecvServ, "^keep-alive:", [global,multiline,caseless]),
    nomatch = re:run(RecvServ, "^proxy-authorization:", [global,multiline,caseless]),
    {match,_} = re:run(RecvServ, "^proxy-authentication:", [global,multiline,caseless]),
    nomatch = re:run(RecvClient, "^te:", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient, "^trailer:", [global,multiline,caseless]),
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
    %% This feature only works is the header isn't fully accumulated in the
    %% buffer yet (otherwise, why cancel it?) -- the TCP buffer size should be
    %% reduced.
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
    Resp = resp_huge_status(1024*1024),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    %% This feature only works is the header isn't fully accumulated in the
    %% buffer yet (otherwise, why cancel it?) -- the TCP buffer size should be
    %% reduced.
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

via(Config) ->
    %% A Proxy should advertise its presence with the 'via'
    %% header both to the client and server.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
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
    {ok, RecvServ} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    {match,_} = re:run(RecvServ, "^via: ?1.1 vegur", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient, "^via: ?1.1 vegur", [global,multiline,caseless]).

via_chain(Config) ->
    %% A Proxy should advertise its presence with the 'via'
    %% header both to the client and server.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_via("1.1 proxy", Config),
    Resp = resp_via("1.1 proxy"),
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
    {match,_} = re:run(RecvServ, "^via: ?1.1 proxy, 1.1 vegur", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient, "^via: ?1.1 proxy, 1.1 vegur", [global,multiline,caseless]).

bad_status(Config) ->
    %% An endpoint app may send custom HTTP Statuses as long as they are
    %% represented by a 3-digit code, with custom text. The custom text
    %% should be passed as-is.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_custom_status(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _RecvServ} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    {match,_} = re:run(RecvClient, "666 THE NUMBER OF THE BEAST", [global,multiline,caseless]).


preserve_case(Config) ->
    %% An endpoint app may send custom HTTP Statuses as long as they are
    %% represented by a 3-digit code, with custom text. The custom text
    %% should be passed as-is.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_altcase(Config),
    Resp = resp_altcase(),
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
    %% Final response checking, all headers keep their case
    ct:pal("SRV: ~p",[RecvServ]),
    ct:pal("CLI: ~p",[RecvClient]),
    nomatch = re:run(RecvServ, "^cONtENT-lEnGTH:", [global,multiline]),
    {match,_} = re:run(RecvServ, "^Content-Length:", [global,multiline]), % we rewrite this one
    {match,_} = re:run(RecvServ, "^Content-Type:", [global,multiline]),
    {match,_} = re:run(RecvServ, "^Connection:", [global,multiline]), % hop-by-hop
    {match,_} = re:run(RecvServ, "^Proxy-Authentication:", [global,multiline]),
    {match,_} = re:run(RecvServ, "^Custom-Header:", [global,multiline]),
    {match,_} = re:run(RecvClient, "^Content-Length:", [global,multiline]), % we don't rewrite but may depending on resp?
    {match,_} = re:run(RecvClient, "^Content-Type:", [global,multiline]),
    {match,_} = re:run(RecvClient, "^Connection:", [global,multiline]), % hop-by-hop
    {match,_} = re:run(RecvClient, "^Proxy-Authentication:", [global,multiline]),
    {match,_} = re:run(RecvClient, "^Custom-Header:", [global,multiline]).

header_order(Config) ->
    %% An endpoint app may send custom HTTP Statuses as long as they are
    %% represented by a 3-digit code, with custom text. The custom text
    %% should be passed as-is.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_header_order(),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _RecvServ} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    {match, [{S1, _}]} = re:run(RecvClient, "test-header: 1",
                                [multiline, caseless]),
    {match, [{S2, _}]} = re:run(RecvClient, "test-header: 2",
                                [multiline, caseless]),
    {match, [{S3, _}]} = re:run(RecvClient, "test-header: 3",
                                [multiline, caseless]),
    true = S1 < S2 andalso S2 < S3.

host_nofold(Config) ->
    %% If >=2 Host values exist but they are the same,
    %% still be angry and error out. See https://tools.ietf.org/html/rfc7230#page-45
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = multi_host_headers(Config, domain(Config), domain(Config)) ++ req_body(),
    %% Send data to the proxy right away.
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Without making it to the server, we get a 400 back. for bad request
    {ok, Resp} = gen_tcp:recv(Client, 0, 1000),
    {match, _} = re:run(Resp, "400").

host_conflict(Config) ->
    %% If >=2 Host values exist and are different, be
    %% angry and error out.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = multi_host_headers(Config, domain(Config), "bad."++domain(Config)) ++ req_body(),
    %% Send data to the proxy right away.
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Without making it to the server, we get a 400 back. for bad request
    {ok, Resp} = gen_tcp:recv(Client, 0, 1000),
    {match, _} = re:run(Resp, "400").

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHUNKED REQUESTS BEHAVIOUR %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
passthrough(Config) ->
    %% Chunked data can move through the stack without being modified.
    %% We *could* re-chunk data, but leaving chunks as is is more
    %% transparent and also easier.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Chunks = "3\r\nabc\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuv\r\n0\r\n\r\n",
    Req = [chunked_headers(Config), Chunks],
    Resp = [resp_headers(chunked), Chunks],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_timeout(Client),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[RecvServ,RecvClient]),
    {match,_} = re:run(RecvServ, Chunks),
    {match,_} = re:run(RecvClient, Chunks),
    wait_for_closed(Server, 500).

passthrough_short_crlf(Config) ->
    %% Chunked data can move through the stack without being modified.
    %% We *could* re-chunk data, but leaving chunks as is is more
    %% transparent and also easier. This will time out instead, waiting
    %% for the final \r\n.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Chunks = "3\r\nabc\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuv\r\n0\r\n",
    Req = [chunked_headers(Config), Chunks],
    Resp = [resp_headers(chunked), Chunks],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_timeout(Client),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[RecvServ,RecvClient]),
    {match,_} = re:run(RecvServ, Chunks),
    0 = iolist_size(RecvClient),
    gen_tcp:close(Client), % we don't wait for a configured timeout
    wait_for_closed(Server, 500).

passthrough_early_0length(Config) ->
    %% Chunked data can move through the stack without being modified.
    %% We *could* re-chunk data, but leaving chunks as is is more
    %% transparent and also easier.
    %% All 0-length chunks interrupt a stream with an error.
    %% This test case verifies the case where the client sends everything at
    %% once and vegur can catch it before the server responds.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Chunks = "3\r\nabc\r\n0\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuv\r\n0\r\n\r\n",
    Req = [chunked_headers(Config), Chunks],
    Resp = [resp_headers(chunked), Chunks],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_close(Server),
    {error, closed} = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_close(Client),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[RecvServ,RecvClient]),
    %% Detected the bad request early on and responded before the server could
    {match, _} = re:run(RecvServ, "3\r\nabc\r\n$"),
    {match, _} = re:run(RecvClient, "400 Bad Request"),
    wait_for_closed(Server, 500).

passthrough_partial_early_0length1(Config) ->
    %% Chunked data can move through the stack without being modified.
    %% We *could* re-chunk data, but leaving chunks as is is more
    %% transparent and also easier.
    %% All 0-length chunks interrupt a stream with an error.
    %% This test case verifies the case where the client sends everything in
    %% parts and vegur can catch it after the server responded, but before the
    %% error in the server response could be caught
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    [Chunks1,Chunks2] = ["3\r\nabc\r\n","0\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuv\r\n0\r\n\r\n"],
    Req = [chunked_headers(Config), Chunks1],
    Resp = [resp_headers(chunked), Chunks1],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ1 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    timer:sleep(100), % just for luck
    ok = gen_tcp:send(Client, Chunks2),
    RecvClient = recv_until_close(Client),
    RecvServ2 = recv_until_close(Server),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[[RecvServ1,RecvServ2],RecvClient]),
    {match, _} = re:run([RecvServ1,RecvServ2], "3\r\nabc\r\n$"),
    {match, _} = re:run(RecvClient, "400 Bad Request").

passthrough_partial_early_0length2(Config) ->
    %% Chunked data can move through the stack without being modified.
    %% We *could* re-chunk data, but leaving chunks as is is more
    %% transparent and also easier.
    %% All 0-length chunks interrupt a stream with an error.
    %% This test case verifies the case where the client sends everything in
    %% parts and vegur can catch it after the server responded, but after the
    %% error in the server response could be caught.
    %% Due to the current vegur model, this is the same; we only parse the
    %% response once the request is done
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    [Chunks1,Chunks2] = ["3\r\nabc\r\n","0\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuv\r\n0\r\n\r\n"],
    Req = [chunked_headers(Config), Chunks1],
    Resp = [resp_headers(chunked), Chunks1],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ1 = recv_until_timeout(Server),
    ok = gen_tcp:send(Client, Chunks2),
    timer:sleep(100), % just for luck
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_close(Client),
    RecvServ2 = recv_until_close(Server),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[[RecvServ1,RecvServ2], RecvClient]),
    {match, _} = re:run([RecvServ1,RecvServ2], "3\r\nabc\r\n$"),
    {match, _} = re:run(RecvClient, "400 Bad Request").

passthrough_partial_early_0length3(Config) ->
    %% Chunked data can move through the stack without being modified.
    %% We *could* re-chunk data, but leaving chunks as is is more
    %% transparent and also easier.
    %% All 0-length chunks interrupt a stream with an error.
    %% This test case verifies the case where the client sends everything
    %% fine and vegur can catch it after the server responded, but after the
    %% error in the server response could be caught.
    %% Due to the current vegur model, this is the same; we only parse the
    %% response once the request is done
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    [Chunks1,Chunks2] = ["3\r\nabc\r\n","0\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuv\r\n0\r\n\r\n"],
    Req = [chunked_headers(Config), Chunks1, "0\r\n\r\n"],
    Resp = [resp_headers(chunked), Chunks1],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient1 = recv_until_timeout(Client),
    ok = gen_tcp:send(Server, Chunks2),
    RecvClient2 = recv_until_close(Client),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[RecvServ,[RecvClient1,RecvClient2]]),
    {match, _} = re:run(RecvServ, "3\r\nabc\r\n0\r\n\r\n$"),
    %% Content was down the line already
    nomatch = re:run([RecvClient1,RecvClient2], "400 Bad Request"),
    {match,_} = re:run(RecvClient1, "200 OK"),
    {match,_} = re:run([RecvClient1, RecvClient2], "3\r\nabc\r\n$"),
    nomatch = re:run([RecvClient1,RecvClient2], "3\r\nabc\r\n0\r\n\r\n$").

interrupted_client(Config) ->
    %% Regression --
    %% Chunked data can move through the stack without being modified.
    %% When the connection is interrupted halfway through, the stack is
    %% still able to close it fine despite having dirty data (like
    %% continuations) in its arguments.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ChunksHalf = "3\r\nabc\r\n5\r\ndefg",
    Req = [chunked_headers(Config), ChunksHalf],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    ok = gen_tcp:close(Client),
    _ = recv_until_close(Server),
    wait_for_closed(Client, 500).

interrupted_server(Config) ->
    %% Regression --
    %% Chunked data can move through the stack without being modified.
    %% When the connection is interrupted halfway through, the stack is
    %% still able to close it fine despite having dirty data (like
    %% continuations) in its arguments.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ChunksHalf = "3\r\nabc\r\n5\r\ndefg",
    Chunks = [ChunksHalf, "h\r\ne\r\nijklmnopqrstuv\r\n0\r\n\r\n"],
    Req = [chunked_headers(Config), Chunks],
    Resp = [resp_headers(chunked), ChunksHalf],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, _RecvServ} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    CloseTime = os:timestamp(),
    ok = gen_tcp:close(Server),
    RecvClient = recv_until_close(Client),
    CloseDetect = os:timestamp(),
    %% Connection closed and may or may not have reported partial chunks
    %% based on timing that would close the socket.
    ct:pal("RecvClient: ~p",[RecvClient]),
    %% The detection of a termination takes place in less than 2 seconds.
    %% Note that on a loopback interface, this should always pass. Remote
    %% interfaces detect and propagate connection termination differently.
    ?assert(timer:seconds(2) > timer:now_diff(CloseDetect,CloseTime)/1000),
    check_stub_error({downstream, closed}).

bad_chunk(Config) ->
    %% A bad chunk from the server cannot be reported on via error codes
    %% due to being part of an already-in-progress response.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Chunks = "3\r\nabc\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuv\r\n0\r\n\r\n",
    BadChunks = "3\r\nabc\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuvkjalnfksl\r\n0\r\n\r\n",
    Req = [chunked_headers(Config), Chunks],
    Resp = [resp_headers(chunked), BadChunks],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_close(Client),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[RecvServ,RecvClient]),
    {match, _} = re:run(RecvClient, "200"),
    nomatch = re:run(RecvClient, "502"), % can't report an error halfway through
    wait_for_closed(Server, 500).

trailers(Config) ->
    %% Trailers are supported, from the server *and* the client.
    %% We do not care to verify whether the client had the
    %% `TE: trailers' header for supporting trailers in the response.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Chunks = "3\r\nabc\r\n0\r\nhead1:val1\r\nhead2: val2\r\n\r\n",
    Req = [chunked_headers_trailers(Config, "head1, head2"), Chunks],
    Resp = [resp_headers({trailers, "head1,head2"}), Chunks],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_timeout(Client),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[RecvServ,RecvClient]),
    {match, _} = re:run(RecvServ, "^trailer: ?head1, ?head2", [global, multiline, caseless]),
    {match, _} = re:run(RecvServ, "head1:val1", [global, multiline, caseless]),
    {match, _} = re:run(RecvServ, "head2: val2", [global, multiline, caseless]),
    {match, _} = re:run(RecvClient, "200 OK"),
    nomatch = re:run(RecvClient, "502"), % can't report an error halfway through
    nomatch = re:run(RecvClient, "400"), % can't report an error halfway through
    {match, _} = re:run(RecvClient, "^trailer: ?head1, ?head2", [global, multiline, caseless]),
    {match, _} = re:run(RecvClient, "head1:val1", [global, multiline, caseless]),
    {match, _} = re:run(RecvClient, "head2: val2", [global, multiline, caseless]),
    wait_for_closed(Server, 500).

trailers_close(Config) ->
    %% Trailers are supported, from the server *and* the client.
    %% We do not care to verify whether the client had the
    %% `TE: trailers' header for supporting trailers in the response.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Chunks = "3\r\nabc\r\n0\r\nhead1:val1\r\nhead2: val2\r\n\r\n",
    Req = [chunked_headers_trailers(Config, "head1, head2", close), Chunks],
    Resp = [resp_headers({trailers, "head1,head2"}), Chunks],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_close(Client),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[RecvServ,RecvClient]),
    {match, _} = re:run(RecvServ, "^trailer: ?head1, ?head2", [global, multiline, caseless]),
    {match, _} = re:run(RecvServ, "head1:val1", [global, multiline, caseless]),
    {match, _} = re:run(RecvServ, "head2: val2", [global, multiline, caseless]),
    {match, _} = re:run(RecvClient, "200 OK"),
    nomatch = re:run(RecvClient, "502"), % can't report an error halfway through
    nomatch = re:run(RecvClient, "400"), % can't report an error halfway through
    {match, _} = re:run(RecvClient, "^trailer: ?head1, ?head2", [global, multiline, caseless]),
    {match, _} = re:run(RecvClient, "head1:val1", [global, multiline, caseless]),
    {match, _} = re:run(RecvClient, "head2: val2", [global, multiline, caseless]),
    wait_for_closed(Server, 500).

trailers_client_err(Config) ->
    %% Bad trailers from a client yields a 400.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Chunks = "3\r\nabc\r\n0\r\ntrailer is invalid\r\n\r\n",
    Req = [chunked_headers_trailers(Config, "head1, head2", close), Chunks],
    Resp = [resp_headers({trailers, "head1,head2"}), Chunks],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    ok = gen_tcp:send(Server, Resp),
    RecvServ = recv_until_close(Server),
    RecvClient = recv_until_close(Client),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[RecvServ,RecvClient]),
    nomatch = re:run(RecvClient, "200 OK"),
    {match,_} = re:run(RecvClient, "400").

trailers_serv_err(Config) ->
    %% A bad trailer from the server cannot be reported on via error codes
    %% due to being part of an already-in-progress response.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Chunks = "3\r\nabc\r\n0\r\ntrailer is invalid\r\n\r\n",
    Req = req(Config),
    Resp = [resp_headers({trailers, "head1,head2"}), Chunks],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_close(Client),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[RecvServ,RecvClient]),
    {match, _} = re:run(RecvClient, "200"),
    nomatch = re:run(RecvClient, "502"), % can't report an error halfway through
    nomatch = re:run(RecvClient, "400"), % can't report an error halfway through
    wait_for_closed(Server, 500).

trailers_no_header(Config) ->
    %% Trailers without the `trailer' header are supported.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Chunks = "3\r\nabc\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuv\r\n0\r\n",
    Trailers = "head: val\r\n\r\n",
    Req = [chunked_headers(Config), Chunks, Trailers],
    Resp = [resp_headers(chunked), Chunks, Trailers],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_timeout(Client),
    %% Check final connection status
    ct:pal("RecvServ: ~p~nRecvClient: ~p",[RecvServ,RecvClient]),
    {match, _} = re:run(RecvServ, Chunks),
    {match, _} = re:run(RecvServ, Trailers),
    {match, _} = re:run(RecvClient, Chunks),
    {match, _} = re:run(RecvClient, Trailers),
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).


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
    %% We override this as is allowed for proxies, given the request isn't
    %% one we need to wait for the body, but if the client specified it wanted
    %% a keep-alive connection (or nothing)
    nomatch = re:run(Recv, "^connection: close", [global,multiline,caseless]),
    {match,_} = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
    %% No content-length header was added
    nomatch = re:run(Recv, "^content-length:", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    [] = recv_until_timeout(Client),
    gen_tcp:close(Client).

head_close_expect_client(Config) ->
    %% Same as above, but for larger bodies getting streamed
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_head_close(Config),
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
    %% We do not override the connection: close if the client requested it
    %% that way first.
    {match,_} = re:run(Recv, "^connection: close", [global,multiline,caseless]),
    nomatch = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    wait_for_closed(Client, 500).

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

valid_status_204(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_204(valid),
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
    {match,_} = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(Recv, "No Content", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)),
    Ref1 = make_ref(),
    start_acceptor(Ref1, Config),
    ok = gen_tcp:send(Client, Req),
    timer:sleep(500),
    Server1 = get_accepted(Ref1),
    {ok, _} = gen_tcp:recv(Server1, 0, 1000),
    ok = gen_tcp:send(Server1, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(Recv, "No Content", [global,multiline,caseless]),
    wait_for_closed(Server1, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

valid_status_304(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = resp_304(valid),
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
    {match,_} = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(Recv, "Not Modified", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)),
    Ref1 = make_ref(),
    start_acceptor(Ref1, Config),
    ok = gen_tcp:send(Client, Req),
    timer:sleep(500),
    Server1 = get_accepted(Ref1),
    {ok, _} = gen_tcp:recv(Server1, 0, 1000),
    ok = gen_tcp:send(Server1, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(Recv, "Not Modified", [global,multiline,caseless]),
    wait_for_closed(Server1, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

valid_head(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_head(valid, Config),
    Resp = resp(head_response, 10000),
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
    {match,_} = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(Recv, "200 OK", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)),
    Ref1 = make_ref(),
    start_acceptor(Ref1, Config),
    ok = gen_tcp:send(Client, Req),
    timer:sleep(500),
    Server1 = get_accepted(Ref1),
    {ok, _} = gen_tcp:recv(Server1, 0, 1000),
    ok = gen_tcp:send(Server1, Resp),
    {ok, Recv} = gen_tcp:recv(Client, 0, 1000),
    {match,_} = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(Recv, "200 OK", [global,multiline,caseless]),
    wait_for_closed(Server1, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

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
    {match,_} = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
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
    ct:pal("Recv: ~p",[Recv]),
    {match,_} = re:run(Recv, "chunked", [global,multiline,caseless]),
    {match,_} = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
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
    %% We're not waiting for the body, so we can skip on the connection:close
    %% and override it with a connection: keepalive if the client specified it
    %% (or nothing)
    nomatch = re:run(Recv, "^connection: close", [global,multiline,caseless]),
    {match,_} = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
    nomatch = re:run(Recv, "body", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

status_close_304_client(Config) ->
    %% For a 304 response, we should not wait for a request body. We will not,
    %% however, strip any headers that are not hop by hop.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_close(Config),
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
    %% We're not waiting for the body, so we can skip on the connection:close
    %% and override it with a connection: keepalive unless the client asked
    %% for it
    {match,_} = re:run(Recv, "^connection: close", [global,multiline,caseless]),
    nomatch = re:run(Recv, "^connection: keep-alive", [global,multiline,caseless]),
    nomatch = re:run(Recv, "body", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

bad_transfer_encoding(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    ReqHeaders = invalid_transfer_encoding(Config),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, ReqHeaders),
    %% No data to exchange because this failed
    {ok, Response} = gen_tcp:recv(Client, 0, 1000),
    %% Final response checking
    {match, _} = re:run(Response, "400").

%%%%%%%%%%%%%%%%%%
%%% LARGE BODY %%%
%%%%%%%%%%%%%%%%%%
large_body_stream(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_large(Config, 8000),
    Resp = resp_large(8000),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_timeout(Client),
    ct:pal("RecvServ: ~p", [RecvServ]),
    ?assert(iolist_size(RecvServ) > 8000),
    ?assert(iolist_size(RecvClient) > 8000),
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

large_body_close(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_large_close(Config, 8000),
    Resp = resp_large(8000),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_close(Client),
    ct:pal("RecvServ: ~p", [RecvServ]),
    ?assert(iolist_size(RecvServ) > 8000),
    ?assert(iolist_size(RecvClient) > 8000),
    wait_for_closed(Server, 500).

large_body_close_delimited(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_large(Config, 8000),
    Resp = resp_large_close_delimited(8000),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    gen_tcp:close(Server),
    RecvClient = recv_until_close(Client),
    ct:pal("RecvServ: ~p", [RecvServ]),
    ?assert(iolist_size(RecvServ) > 8000),
    ?assert(iolist_size(RecvClient) > 8000).


large_body_request_response_interrupt(Config) ->
    %% When transfering a large body from a client to the back-end, the
    %% back-end may respond and close the connection early. This will in
    %% turn yield a case where the proxy should detect the data on the
    %% line and return it to the client.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    <<Req1:8000/binary, Req2:4000/binary, _/binary>> = iolist_to_binary(req_large(Config, 16000)),
    Resp = resp_304(close),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req1),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    _ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    %% slowly trickle upload data to make sure we receive the response
    [ok = gen_tcp:send(Client, <<Byte>>) || <<Byte>> <= Req2, ok == timer:sleep(1)],
    %% Close the connection
    gen_tcp:close(Server),
    %% We should get a response back even if we didn't finish sending it
    Recv = recv_until_close(Client),
    %% Check results
    ct:pal("Resp: ~p", [Recv]),
    {match,_} = re:run(Recv, "^HTTP/1.1 304 OK", [global,multiline,caseless]).

large_chunked_request_response_interrupt(Config) ->
    %% When transfering a large body from a client to the back-end, the
    %% back-end may respond and close the connection early. This will in
    %% turn yield a case where the proxy should detect the data on the
    %% line and return it to the client.
    %% Same as the other, but chunked.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req1 = chunked_headers(Config),
    Req2 = iolist_to_binary(lists:duplicate(50, "3\r\nabc\r\n5\r\ndefg")), % never finish
    Resp = resp_304(close),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req1),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    _ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    %% slowly trickle upload data to make sure we receive the response
    [ok = gen_tcp:send(Client, <<Byte>>) || <<Byte>> <= Req2, ok == timer:sleep(1)],
    %% Close the connection
    gen_tcp:close(Server),
    %% We should get a response back even if we didn't finish sending it
    Recv = recv_until_close_long(Client),
    %% Check results
    ct:pal("Resp: ~p", [Recv]),
    {match,_} = re:run(Recv, "^HTTP/1.1 304 OK", [global,multiline,caseless]).

large_close_request_response_interrupt(Config) ->
    %% When transfering a large body from a client to the back-end, the
    %% back-end may respond and close the connection early. This will in
    %% turn yield a case where the proxy should detect the data on the
    %% line and return it to the client.
    %% Same as before, but using a close-delimited response
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    <<Req1:8000/binary, Req2:4000/binary, _/binary>> = iolist_to_binary(req_large(Config, 16000)),
    Resp = resp_large_close_delimited(30000),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req1),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    _ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    %% slowly trickle upload data to make sure we receive the response
    [ok = gen_tcp:send(Client, <<Byte>>) || <<Byte>> <= Req2, ok == timer:sleep(1)],
    %% Close the connection
    gen_tcp:close(Server),
    %% We should get a response back even if we didn't finish sending it
    Recv = recv_until_close(Client),
    %% Check results
    {match,_} = re:run(Recv, "^HTTP/1.1 200 OK", [global,multiline,caseless]),
    ?assert(30000 =< byte_size(iolist_to_binary(Recv))).

large_close_request_response_interrupt_undetected(Config) ->
    %% When transfering a large body from a client to the back-end, the
    %% back-end may respond and close the connection early. This will in
    %% turn yield a case where the proxy should detect the data on the
    %% line and return it to the client.
    %% However, if the body size ends up being greater than what the buffer
    %% can accumulate, the detection falls apart and the response cannot
    %% detect the termination in a surefire way (this may depend on
    %% the platform?)
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    <<Req1:8000/binary, Req2:4000/binary, Req3/binary>> = iolist_to_binary(req_large(Config, 16000)),
    Resp = resp_large_close_delimited(80000),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req1),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    _ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    %% slowly trickle upload data to make sure we receive the response
    [ok = gen_tcp:send(Client, <<Byte>>) || <<Byte>> <= Req2, ok == timer:sleep(1)],
    %% Close the connection
    gen_tcp:close(Server),
    %% We shouldn't get a response because we filled the buffer on the other end,
    %% but only one some TCP stacks. Depending on this, we have different
    %% possible endings -- either a timeout or a closed connection, depending on
    %% if you're working with a remote host or on localhost.
    Recv = case recv_until_close_or_timeout(Client) of
        [] -> % This is a timeout
            %% Send the rest, get the response
            gen_tcp:send(Client, Req3),
            recv_until_close(Client);
        Data -> % this is a close
            Data
    end,
    %% Check results
    %% The buffer is set to 64kb, so the response relayed will work, but
    %% be broken at the end. This is expected.
    {match,_} = re:run(Recv, "^HTTP/1.1 200 OK", [global,multiline,caseless]),
    %% we can't know how much we received except that it was >= than
    %% the max buffer size
    ?assert(65536 =< byte_size(iolist_to_binary(Recv))).

weird_content_length(Config) ->
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
    Resp = [resp_headers("208062 "), lists:duplicate(208062, $a)],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_close(Client),
    ct:pal("RecvServ: ~p", [RecvServ]),
    ct:pal("RecvClient: ~p", [RecvClient]),
    check_stub_error({downstream, content_length}),
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% KEEPALIVE TO THE BACKENDS BEHAVIOUR %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close_close_close(Config) ->
    %% By default, all connections are closed after each request,
    %% doubly do if requested by the back-end.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_close(Config),
    Resp = resp_custom_headers("connection", "close"),
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
    %% Check final connection status
    {match,_} = re:run(RecvServ, "^connection: ?close", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient, "^connection: ?close", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    wait_for_closed(Client, 500),
    Config.

close_keepalive_keepalive(Config) ->
    %% When specified, all connections are closed after each request, despite
    %% everyone asking for a keepalive.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_keepalive(Config),
    Resp = resp_custom_headers("connection", "keep-alive"),
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
    %% We relay a closed connection to the back-end
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Check final connection status
    %% The headers to the client can remain keepalive, but those to
    %% the backend have to be closed.
    {match,_} = re:run(RecvServ, "^connection: ?close", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient, "^connection: ?keep-alive", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

close_default_default(Config) ->
    %% By default, all connections are closed after each request. this remains
    %% true when connection are implicitly keep-alive (by not specifying
    %% anything)
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
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
    {ok, RecvServ} = gen_tcp:recv(Server, 0, 1000),
    %% We relay a closed connection to the back-end
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Check final connection status
    %% The headers to the client can remain keepalive, but those to
    %% the backend have to be closed.
    {match,_} = re:run(RecvServ, "^connection: ?close", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient, "^connection: ?keep-alive", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

close_close_keepalive(Config) ->
    %% When specified, all connections are closed after each request, despite
    %% everyone asking for a keepalive.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_close(Config),
    Resp = resp_custom_headers("connection", "keep-alive"),
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
    %% We relay a closed connection to the back-end
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Check final connection status
    %% The headers to the client are closed as requested,
    %% those to the backend have to be closed.
    {match,_} = re:run(RecvServ, "^connection: ?close", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient, "^connection: ?close", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    wait_for_closed(Client, 500).

close_keepalive_close(Config) ->
    %% When specified, all connections are closed after each request, despite
    %% everyone asking for a keepalive.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_keepalive(Config),
    Resp = resp_custom_headers("connection", "close"),
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
    %% We relay a closed connection to the back-end
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient} = gen_tcp:recv(Client, 0, 1000),
    %% Check final connection status
    %% The headers to the client are keepalive as requested,
    %% those to the backend are closed as requested
    {match,_} = re:run(RecvServ, "^connection: ?close", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient, "^connection: ?keep-alive", [global,multiline,caseless]),
    wait_for_closed(Server, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

keepalive_close_close(Config) ->
    %% By default, all connections are keepalive after each request,
    %% but if they're requested closed, they will be so.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_close(Config),
    Resp = resp_custom_headers("connection", "close"),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient = recv_until_close(Client),
    %% Check final connection status
    {match,_} = re:run(RecvServ, "^connection: ?close", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient, "^connection: ?close", [global,multiline,caseless]),
    wait_for_closed(Server, 500).

keepalive_keepalive_keepalive(Config) ->
    %% By default, all connections are keepalive, and we respect that
    %% when all parties request so.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_keepalive(Config),
    Resp = resp_custom_headers("connection", "keep-alive"),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ1 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient1 = recv_until_timeout(Client),
    %% Check final connection status
    {match,_} = re:run(RecvServ1, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient1, "^connection: ?keep-alive", [global,multiline,caseless]),
    %% Second round of calls, everything is still up
    ok = gen_tcp:send(Client, Req),
    RecvServ2 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient2 = recv_until_timeout(Client),
    {match,_} = re:run(RecvServ2, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient2, "^connection: ?keep-alive", [global,multiline,caseless]),
    ?assertError(not_closed, wait_for_closed(Server, 500)),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

keepalive_default_default(Config) ->
    %% By default, all connections are keepalive, and we respect that
    %% when all parties request so implicitly through default HTTP
    %% header values
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req(Config),
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
    RecvServ1 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient1 = recv_until_timeout(Client),
    %% Check final connection status
    {match,_} = re:run(RecvServ1, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient1, "^connection: ?keep-alive", [global,multiline,caseless]),
    %% Second round of calls, everything is still up
    ok = gen_tcp:send(Client, Req),
    RecvServ2 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient2 = recv_until_timeout(Client),
    {match,_} = re:run(RecvServ2, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient2, "^connection: ?keep-alive", [global,multiline,caseless]),
    ?assertError(not_closed, wait_for_closed(Server, 500)),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

keepalive_close_keepalive(Config) ->
    %% By default, all connections are keepalive, but if the client requests
    %% it as closed, we obliged down to the back-end.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_close(Config),
    Resp = resp_custom_headers("connection", "keep-alive"),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    {ok, RecvServ1} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, Resp),
    {ok, RecvClient1} = gen_tcp:recv(Client, 0, 1000),
    %% Check final connection status
    {match,_} = re:run(RecvServ1, "^connection: ?close", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient1, "^connection: ?close", [global,multiline,caseless]),
    %% Second round of calls, everything is closed
    wait_for_closed(Server, 500),
    wait_for_closed(Client, 500).

keepalive_keepalive_close(Config) ->
    %% By default, all connections are keepalive, but whenever the backend
    %% asks for a closed connection, only the connection to the front-end
    %% client remains active.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_keepalive(Config),
    Resp = resp_custom_headers("connection", "close"),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref1 = make_ref(),
    start_acceptor(Ref1, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server1 = get_accepted(Ref1),
    %% Exchange all the data
    {ok, RecvServ1} = gen_tcp:recv(Server1, 0, 1000),
    ok = gen_tcp:send(Server1, Resp),
    {ok, RecvClient1} = gen_tcp:recv(Client, 0, 1000),
    %% Check final connection status
    %% The server received a keep-alive message, sent back a close header. Vegur
    %% middlemans the hell out of this and shows a keep-alive to the client.
    {match,_} = re:run(RecvServ1, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient1, "^connection: ?keep-alive", [global,multiline,caseless]),
    wait_for_closed(Server1, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)),
    %% Second round of calls, frontend is still up, but the back-end connection
    %% has been closed. We reopen a new back-end connection but keep the
    %% front-end going for similar results.
    ct:pal("round 2!"),
    Ref2 = make_ref(),
    start_acceptor(Ref2, Config),
    ok = gen_tcp:send(Client, Req),
    Server2 = get_accepted(Ref2),
    %% re-exchange data
    {ok, RecvServ2} = gen_tcp:recv(Server2, 0, 1000),
    ok = gen_tcp:send(Server2, Resp),
    RecvClient2 = recv_until_timeout(Client),
    {match,_} = re:run(RecvServ2, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient2, "^connection: ?keep-alive", [global,multiline,caseless]),
    wait_for_closed(Server2, 500),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

keepalive_reset(Config) ->
    %% When a Keepalive connection hits a 'new' tuple instead of 'default', the
    %% old connection is dropped and the new one is kept as a keepalive.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_keepalive(Config),
    Resp = resp_custom_headers("connection", "keep-alive"),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref1 = make_ref(),
    start_acceptor(Ref1, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server1 = get_accepted(Ref1),
    %% Exchange all the data
    RecvServ1 = recv_until_timeout(Server1),
    ok = gen_tcp:send(Server1, Resp),
    RecvClient1 = recv_until_timeout(Client),
    %% Check final connection status
    {match,_} = re:run(RecvServ1, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient1, "^connection: ?keep-alive", [global,multiline,caseless]),
    %% Second round of calls, everything is still up
    ok = gen_tcp:send(Client, Req),
    RecvServ2 = recv_until_timeout(Server1),
    ok = gen_tcp:send(Server1, Resp),
    RecvClient2 = recv_until_timeout(Client),
    {match,_} = re:run(RecvServ2, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient2, "^connection: ?keep-alive", [global,multiline,caseless]),
    %?assertError(not_closed, wait_for_closed(Server, 500)),
    %?assertError(not_closed, wait_for_closed(Client, 500)),
    % Start a new acceptor as the next connection should be fresh but also keepalive!
    Ref2 = make_ref(),
    start_acceptor(Ref2, Config),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server2 = get_accepted(Ref2),
    wait_for_closed(Server1, 500),
    %% Exchange all the data
    RecvServ3 = recv_until_timeout(Server2),
    ok = gen_tcp:send(Server2, Resp),
    RecvClient3 = recv_until_timeout(Client),
    %% Check final connection status
    {match,_} = re:run(RecvServ3, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient3, "^connection: ?keep-alive", [global,multiline,caseless]),
    %% Second round of calls, everything is still up
    ok = gen_tcp:send(Client, Req),
    RecvServ4 = recv_until_timeout(Server2),
    ok = gen_tcp:send(Server2, Resp),
    RecvClient4 = recv_until_timeout(Client),
    {match,_} = re:run(RecvServ4, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient4, "^connection: ?keep-alive", [global,multiline,caseless]),
    ?assertError(not_closed, wait_for_closed(Server2, 500)),
    ?assertError(not_closed, wait_for_closed(Client, 500)).


keepalive_http_1_0(Config) ->
    %% By default, all connections are keepalive, and we respect that
    %% when all parties request so.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = [http_1_0_keepalive_headers(Config), req_body()],
    Resp = resp_custom_headers("connection", "keep-alive"),
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ1 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient1 = recv_until_timeout(Client),
    %% Check final connection status
    {match,_} = re:run(RecvServ1, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient1, "^connection: ?keep-alive", [global,multiline,caseless]),
    %% Second round of calls, everything is still up
    ok = gen_tcp:send(Client, Req),
    RecvServ2 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient2 = recv_until_timeout(Client),
    {match,_} = re:run(RecvServ2, "^connection: ?keep-alive", [global,multiline,caseless]),
    {match,_} = re:run(RecvClient2, "^connection: ?keep-alive", [global,multiline,caseless]),
    ?assertError(not_closed, wait_for_closed(Server, 500)),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

keepalive_upgrade_close_server(Config) ->
    %% Even with all settings going for keepalive, upgraded connections close
    %% on both ends of the proxy
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
    _ReqHeaders = recv_until_timeout(Server),
    %% Send the 100 Continue, then the body
    ok = gen_tcp:send(Server, Resp100),
    ok = gen_tcp:send(Client, ReqBody),
    ReqBody = lists:flatten(recv_until_timeout(Server)),
    %% Send the 101 back
    ok = gen_tcp:send(Server, Resp101),
    %% match more broadly -- headers can be added here
    {ok, "HTTP/1.1 100 "++_} = gen_tcp:recv(Client, 0, 1000),
    {ok, "HTTP/1.1 101 "++_} = gen_tcp:recv(Client, 0, 1000),
    %% Now we're in free roam (screw websockets har har!)
    ok = gen_tcp:send(Client, "ping"),
    {ok, "ping"} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, "pong"),
    {ok, "pong"} = gen_tcp:recv(Client, 0, 1000),
    gen_tcp:close(Server),
    wait_for_closed(Client, 5000).

keepalive_upgrade_close_client(Config) ->
    %% Even with all settings going for keepalive, upgraded connections close
    %% on both ends of the proxy
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
    _ReqHeaders = recv_until_timeout(Server),
    %% Send the 100 Continue, then the body
    ok = gen_tcp:send(Server, Resp100),
    ok = gen_tcp:send(Client, ReqBody),
    ReqBody = lists:flatten(recv_until_timeout(Server)),
    %% Send the 101 back
    ok = gen_tcp:send(Server, Resp101),
    %% match more broadly -- headers can be added here
    {ok, "HTTP/1.1 100 "++_} = gen_tcp:recv(Client, 0, 1000),
    {ok, "HTTP/1.1 101 "++_} = gen_tcp:recv(Client, 0, 1000),
    %% Now we're in free roam (screw websockets har har!)
    ok = gen_tcp:send(Client, "ping"),
    {ok, "ping"} = gen_tcp:recv(Server, 0, 1000),
    ok = gen_tcp:send(Server, "pong"),
    {ok, "pong"} = gen_tcp:recv(Client, 0, 1000),
    gen_tcp:close(Client),
    wait_for_closed(Server, 5000).

keepalive_chunked(Config) ->
    %% Keepalive works with chunked requests and responses
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Chunks = "3\r\nabc\r\n5\r\ndefgh\r\ne\r\nijklmnopqrstuv\r\n0\r\n\r\n",
    Req = [chunked_keepalive_headers(Config), Chunks],
    Resp = [resp_headers(chunked_keepalive), Chunks],
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ1 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient1 = recv_until_timeout(Client),
    %% Check final connection status
    {match,_} = re:run(RecvServ1, Chunks),
    {match,_} = re:run(RecvClient1, Chunks),
    %% Exchange all the data AGAIN
    ok = gen_tcp:send(Client, Req),
    RecvServ2 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient2 = recv_until_timeout(Client),
    %% Check final connection status
    {match,_} = re:run(RecvServ2, Chunks),
    {match,_} = re:run(RecvClient2, Chunks).

keepalive_close_delimited(Config) ->
    %% Close-delimited answers force both ends of the connection
    %% to be closed no matter the keepalive settings.
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_large(Config, 8000), % implicit keepalive
    Resp = resp_large_close_delimited(8000), % explicit close
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref = make_ref(),
    start_acceptor(Ref, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref),
    %% Exchange all the data
    RecvServ = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    gen_tcp:close(Server),
    RecvClient = recv_until_close(Client),
    ?assert(iolist_size(RecvServ) > 8000),
    ?assert(iolist_size(RecvClient) > 8000),
    wait_for_closed(Server, 500).

keepalive_large_body(Config) ->
    %% Keepalive to the backend works with large body streams
    IP = ?config(server_ip, Config),
    Port = ?config(proxy_port, Config),
    Req = req_large(Config, 8000), % implicit keepalive
    Resp = resp_large(8000), % implicit keepalive
    %% Open the server to listening. We then need to send data for the
    %% proxy to get the request and contact a back-end
    Ref1 = make_ref(),
    start_acceptor(Ref1, Config),
    {ok, Client} = gen_tcp:connect(IP, Port, [{active,false},list],1000),
    ok = gen_tcp:send(Client, Req),
    %% Fetch the server socket
    Server = get_accepted(Ref1),
    %% Exchange all the data
    RecvServ1 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient1 = recv_until_timeout(Client),
    ?assert(iolist_size(RecvServ1) > 8000),
    ?assert(iolist_size(RecvClient1) > 8000),
    %% Round 2
    ct:pal("round 2"),
    %% Exchange all the data
    ok = gen_tcp:send(Client, Req),
    RecvServ2 = recv_until_timeout(Server),
    ok = gen_tcp:send(Server, Resp),
    RecvClient2 = recv_until_timeout(Client),
    ?assert(iolist_size(RecvServ2) > 8000),
    ?assert(iolist_size(RecvClient2) > 8000),
    ?assertError(not_closed, wait_for_closed(Server, 500)),
    ?assertError(not_closed, wait_for_closed(Client, 500)).

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

recv_until_timeout(Port) ->
    case gen_tcp:recv(Port, 0, 100) of
        {error, timeout} -> [];
        {ok, Data} -> [Data | recv_until_timeout(Port)]
    end.

%% Only use this if you can't know how a connection will terminate.
%% This may happen when the differences between running on a loopback
%% interface or a more standard IP end up triggering TCP stack
%% optimizations in the OS that change the expected behaviour.
recv_until_close_or_timeout(Port) ->
    case gen_tcp:recv(Port, 0, 100) of
        {error, timeout} -> [];
        {error, closed} -> [];
        {ok, Data} -> [Data | recv_until_close_or_timeout(Port)]
    end.

%% Wait up to 55s for the termination -- useful for starvation test
%% on a non-loopback interface
recv_until_close_long(Port) -> recv_until_close_long(Port, 550, 0).

recv_until_close_long(_, N, N) -> [];
recv_until_close_long(Port, Max, N) ->
    case gen_tcp:recv(Port, 0, 100) of
        {error, closed} -> [];
        {error, timeout} -> recv_until_close_long(Port, Max, N+1);
        {ok, Data} -> [Data | recv_until_close_long(Port, Max, 0)]
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
    custom_expect_headers(Config, "100-continue").

custom_expect_headers(Config, Value) ->
    "POST /continue-1 HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10\r\n"
    "Expect: "++Value++"\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n".

chunked_headers(Config) ->
    "POST /chunked HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10\r\n" % should be ignored for chunked
    "Transfer-encoding: chunked\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n".

chunked_keepalive_headers(Config) ->
    "POST /chunked HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10\r\n" % should be ignored for chunked
    "Transfer-encoding: chunked\r\n"
    "Content-Type: text/plain\r\n"
    "Connection: keep-alive\r\n"
    "\r\n".

chunked_headers_trailers(Config, Trailers) ->
    "POST /chunked HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10\r\n" % should be ignored for chunked
    "Transfer-encoding: chunked\r\n"
    "Content-Type: text/plain\r\n"
    "Trailer: "++Trailers++"\r\n"
    "\r\n".

chunked_headers_trailers(Config, Trailers, close) ->
    "POST /chunked HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 10\r\n" % should be ignored for chunked
    "Transfer-encoding: chunked\r\n"
    "Content-Type: text/plain\r\n"
    "connection: close\r\n"
    "Trailer: "++Trailers++"\r\n"
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

multi_host_headers(_Config, Domain1, Domain2) ->
    "POST /continue-1 HTTP/1.1\r\n"
    "Host: "++Domain1++"\r\n"
    "Content-Length: 10\r\n"
    "Host: "++Domain2++"\r\n"
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

req_via(Val, Config) ->
    "POST / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 5\r\n"
    "Content-Type: text/plain\r\n"
    "via: "++Val++"\r\n"
    "\r\n"
    "12345".

req_close(Config) ->
    "POST / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 5\r\n"
    "Content-Type: text/plain\r\n"
    "Connection: close\r\n"
    "\r\n"
    "12345".

req_keepalive(Config) ->
    "POST / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 5\r\n"
    "Content-Type: text/plain\r\n"
    "Connection: keep-alive\r\n"
    "\r\n"
    "12345".

req_head(Config) ->
    "HEAD / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 5\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n"
    "12345".
req_head(valid, Config) ->
    "HEAD / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "\r\n".

req_head_close(Config) ->
    "HEAD / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: 5\r\n"
    "Content-Type: text/plain\r\n"
    "Connection: close\r\n"
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

req_altcase(Config) ->
    "POST / HTTP/1.1\r\n"
    "HosT: "++domain(Config)++"\r\n"
    "ConTent-LeNgth: 5\r\n"
    "ConTent-TyPe: text/plain\r\n"
    %% some hop by hop headers
    "ConneCtion: keep-alive\r\n"
    "proxy-AuthentiCation: 0\r\n"
    "CuStOm-HeAdEr: 0\r\n"
    "\r\n"
    "12345".

req_large(Config, Size) ->
    "POST / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: "++integer_to_list(Size)++"\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n" ++
    lists:duplicate(Size, $a).

req_large_close(Config, Size) ->
    "POST / HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Content-Length: "++integer_to_list(Size)++"\r\n"
    "Content-Type: text/plain\r\n"
    "Connection: close\r\n"
    "\r\n" ++
    lists:duplicate(Size, $a).

resp() ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 43\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n".
resp(head_response, ContentLength) ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: "++integer_to_list(ContentLength)++"\r\n"
    "\r\n".

resp_via(Val) ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 43\r\n"
    "Via: "++Val++"\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n".

resp_custom_status() ->
    "HTTP/1.1 666 THE NUMBER OF THE BEAST\r\n"
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
resp_headers(chunked_keepalive) ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Connection: keep-alive\r\n"
    "Transfer-Encoding: chunked\r\n"
    "\r\n";
resp_headers({trailers,Trailers}) ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Transfer-Encoding: chunked\r\n"
    "trailer: "++Trailers++"\r\n"
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

resp_large(Size) ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: "++integer_to_list(Size)++"\r\n"
    "\r\n"++
    lists:duplicate(Size, $a).

resp_large_close_delimited(Size) ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n"++
    lists:duplicate(Size, $a).

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
    "Server: cowboy\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n".

resp_altcase() ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "cONtENT-tYpE: text/plain\r\n"
    "cONtENT-lEnGTH: 43\r\n"
    %% some hop by hop headers
    "ConnEction: keep-alive\r\n"
    "keEp-alive: timeout=213\r\n"
    "proXy-Authentication: 0\r\n"
    "ServEr: cowboy\r\n"
    "cUsToM-hEaDeR: 0\r\n"
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
    Status = lists:duplicate(Len, $X),
    "HTTP/1.1 200 "++Status++"\r\n"
    "Content-Length: 43\r\n"
    "\r\n"
    "abcdefghijklmnoprstuvwxyz1234567890abcdef\r\n".

resp_204(chunked) ->
    "HTTP/1.1 204 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "transfer-encoding: chuNked\r\n"
        "\r\n";
resp_204(valid) ->
    "HTTP/1.1 204 No Content\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
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
    "misplaced body with a bigger but wrong size";
resp_304(valid) ->
    "HTTP/1.1 304 Not Modified\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "\r\n".

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

invalid_transfer_encoding(Config) ->
    "POST /invalid_transfer_encoding HTTP/1.1\r\n"
    "Host: "++domain(Config)++"\r\n"
    "Transfer-Encoding: ,\r\n"
    "Content-Type: text/plain\r\n"
    "\r\n".

resp_header_order() ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Length: 0\r\n"
    "test-header: 1\r\n"
    "test-header: 2\r\n"
    "test-header: 3\r\n"
    "\r\n".

domain(Config) ->
    LPort = ?config(server_port, Config),
    TxtIp = ?config(txt_ip, Config),
    binary_to_list(<<TxtIp/binary, ":", (integer_to_binary(LPort))/binary>>).

check_stub_error(Pattern) ->
    Local = [Args || {_, {vegur_stub, error_page, Args}, _Ret} <- meck:history(vegur_stub)],
    ct:pal("Local: ~p~n", [Local]),
    Pattern = hd(lists:last(Local)).

