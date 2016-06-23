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
-module(vegur_proxy_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, http}
    ,{group, https}
    ].

groups() ->
    [{http, [], [{group, vegur_proxy_headers},
                 {group, vegur_proxy_connect}]},
     {https, [], [{group, vegur_proxy_headers},
                  {group, vegur_proxy_connect}]},
     {vegur_proxy_headers, [], [request_id
                                ,forwarded_for
                                ,via
                                ,connect_time_header
                                ,start_time_header
                                ,route_time_header
                                ,host
                                ,query_string
                                ,content_length
                                ,no_content_length
                                ,custom_downstream_headers
                               ]}
     ,{vegur_proxy_connect, [], [service_try_again
                                ,response_attributes
                                ,request_statistics
                                ,request_keepalive_statistics
                                ,websockets
                                ]}
    ].

init_per_suite(Config) ->
    application:load(vegur),
    application:load(websocket_client),
    meck:new(vegur_stub, [no_link, passthrough]),
    {ok, Cowboyku} = application:ensure_all_started(cowboyku),
    {ok, Inets} = application:ensure_all_started(inets),
    {ok, Meck} = application:ensure_all_started(meck),
    [{started, Cowboyku++Inets++Meck}|Config].

end_per_suite(Config) ->
    application:unload(vegur),
    application:unload(websocket_client),
    [application:stop(App) || App <- ?config(started, Config)],
    Config.

init_per_group(http, Config) ->
    VegurPort = 9333,
    {ok, _} = vegur:start_http(VegurPort, vegur_stub, []),
    mock_through(),
    [{vegur_port, VegurPort}, {scheme, "http"} | Config];
init_per_group(https, Config) ->
    VegurPort = 9333,
    SSLOptions = [{certfile, ?config(data_dir, Config) ++ "/localhost.crt"},
                  {keyfile,  ?config(data_dir, Config) ++ "/localhost.key"},
                  {versions, ['tlsv1.2']}],
    {ok, _} = vegur:start_https(VegurPort, vegur_stub, [], SSLOptions),
    mock_through(),
    [{vegur_port, VegurPort}, {scheme, "https"} | Config];
init_per_group(vegur_proxy_headers, Config) ->
    BackendPort = 9555,
    mock_backend(9555),
    [{backend_port, BackendPort} | Config];
init_per_group(vegur_proxy_connect, Config) ->
    BackendPort = 9555,
    mock_backend(9555),
    [{backend_port, BackendPort} | Config].

end_per_group(http, Config) ->
    vegur:stop_http(),
    Config;
end_per_group(https, Config) ->
    vegur:stop_https(),
    Config;
end_per_group(_, Config) ->
    meck:unload(),
    Config.

init_per_testcase(request_keepalive_statistics, Config) ->
    %% force jitter on some operation to ensure stats vary between reqs
    meck:new(vegur_headers, [passthrough]),
    meck:expect(vegur_headers, request_headers,
                fun(A,B,C) ->
                        timer:sleep(rand:uniform(20)),
                        meck:passthrough([A,B,C])
                end),
    mock_keepalive_backend(?config(backend_port, Config)),
    init_per_testcase(make_ref(), Config);
init_per_testcase(response_attributes, Config) ->
    %% Same as regular setup, but with custom cookies and
    %% headers to test
    BackendPort = ?config(backend_port, Config),
    Self = self(),
    Headers = [{<<"Set-Cookie">>, <<"my=cookie;">>},
               {<<"remote-host">>, <<"localhost">>},
               {<<"X-Tst">>, <<"1337">>}],
    Backend = start_backend(BackendPort, [{in_handle, fun(Req0) ->
                                                     {ok, Req} = cowboyku_req:reply(200, Headers, Req0),
                                                     Self ! {req, Req},
                                                     Req
                                             end}]),
    [{backend, Backend} | Config];
init_per_testcase(_TestCase, Config) ->
    BackendPort = ?config(backend_port, Config),
    Self = self(),
    Backend = start_backend(BackendPort, [{in_handle, fun(Req) ->
                                                     Self ! {req, Req},
                                                     Req
                                             end}]),
    [{backend, Backend} | Config].

end_per_testcase(request_keepalive_statistics, Config) ->
    meck:unload(vegur_headers),
    mock_backend(?config(backend_port, Config)),
    end_per_testcase(make_ref(), Config);
end_per_testcase(_TestCase, Config) ->
    stop_backend(),
    Config.

url(Config) ->
    ?config(scheme, Config) ++
        "://127.0.0.1:" ++
        integer_to_list(?config(vegur_port, Config)).

%%%===================================================================
%%% Test cases
%%%===================================================================
request_id(Config) ->
    Url = url(Config),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {RequestId, _} = cowboyku_req:header(vegur_utils:config(request_id_name), Req),
            valid = erequest_id:validate(RequestId, vegur_utils:config(request_id_max_size))
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {binary_to_list(vegur_utils:config(request_id_name)),
                                                           "testid-with-a-valid-length"}]}, [], []),
    receive
        {req, Req1} ->
            {<<"testid-with-a-valid-length">>, _} = cowboyku_req:header(vegur_utils:config(request_id_name), Req1)
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {binary_to_list(vegur_utils:config(request_id_name)),
                                                           "testid??"}]}, [], []),
    receive
        {req, Req2} ->
            {RequestId1, _} = cowboyku_req:header(vegur_utils:config(request_id_name), Req2),
            true = RequestId1 /= <<"testid??">>,
            valid = erequest_id:validate(RequestId1, vegur_utils:config(request_id_max_size))
    after 5000 ->
            throw(timeout)
    end,
    Config.

forwarded_for(Config) ->
    meck:expect(vegur_utils, peer_ip_port,
                fun(Req) ->
                        {{PeerIp, _}, Req1} = cowboyku_req:peer(Req),
                        {Port, Req2} = cowboyku_req:port(Req1),
                        {{PeerIp, 1234, Port}, Req2}
                   end),
    meck:expect(vegur_stub, feature, fun(peer_port, S) ->
                                             {enabled, S};
                                        (_, S) ->
                                             {disabled, S}
                                     end),
    Port = ?config(vegur_port, Config),
    Url = url(Config),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost:"++integer_to_list(Port)}]}, [], []),
    receive
        {req, Req} ->
            {<<"127.0.0.1">>, _} = cowboyku_req:header(<<"x-forwarded-for">>, Req),
            {DestPort, _} = cowboyku_req:header(<<"x-forwarded-port">>, Req),
            {<<"1234">>, _} = cowboyku_req:header(<<"x-forwarded-peer-port">>, Req),
            Port = list_to_integer(binary_to_list(DestPort))
    after 5000 ->
            throw(timeout)
    end,

    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {"x-forwarded-for", "10.0.0.1"}]}, [], []),
    receive
        {req, Req1} ->
            {<<"10.0.0.1, 127.0.0.1">>, _} = cowboyku_req:header(<<"x-forwarded-for">>, Req1),
            case ?config(scheme, Config) of
                "http" ->
                    {<<"80">>, _} = cowboyku_req:header(<<"x-forwarded-port">>, Req1),
                    {<<"http">>, _} = cowboyku_req:header(<<"x-forwarded-proto">>, Req1);
                "https" ->
                    {<<"443">>, _} = cowboyku_req:header(<<"x-forwarded-port">>, Req1),
                    {<<"https">>, _} = cowboyku_req:header(<<"x-forwarded-proto">>, Req1)
            end
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost:443"}]}, [], []),
    receive
        {req, Req2} ->
            {<<"443">>, _} = cowboyku_req:header(<<"x-forwarded-port">>, Req2),
            {<<"https">>, _} = cowboyku_req:header(<<"x-forwarded-proto">>, Req2)
    after 5000 ->
            throw(timeout)
    end,

    %% Pass a tuple that represents the handler state to enable router_metrics.
    %% We don't have access to the record definition in the tests so do it manually
    meck:expect(vegur_stub, additional_headers,
                fun(Direction, Log, Req, _HandlerState) -> meck:passthrough([Direction, Log, Req, {state, 0, [router_metrics]}]) end),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req3} ->
            {<<"vegur_stub">>, _} = cowboyku_req:header(<<"instance-name">>, Req3),
            {ConnectTime, _} = cowboyku_req:header(<<"connect-time">>, Req3),
            true = is_integer(list_to_integer(binary_to_list(ConnectTime))),
            {TotalRouteTime, _} = cowboyku_req:header(<<"total-route-time">>, Req3),
            true = is_integer(list_to_integer(binary_to_list(TotalRouteTime)))
    after 5000 ->
            throw(timeout)
    end,

    meck:expect(vegur_stub, feature, fun(peer_port, S) ->
                                             {disabled, S};
                                        (_, S) ->
                                             {disabled, S}
                                     end),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost:"++integer_to_list(Port)}]}, [], []),
    receive
        {req, Req4} ->
            {undefined, _} = cowboyku_req:header(<<"x-forwarded-peer-port">>, Req4)
    after 5000 ->
            throw(timeout)
    end,
    Config.

via(Config) ->
    Url = url(Config),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {<<"1.1 vegur">>, _} = cowboyku_req:header(<<"via">>, Req)
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {"via", "happyproxy"}]}, [], []),
    receive
        {req, Req1} ->
            {<<"happyproxy, 1.1 vegur">>, _} = cowboyku_req:header(<<"via">>, Req1)
    after 5000 ->
            throw(timeout)
    end,
    Config.

start_time_header(Config) ->
    Url = url(Config),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {Res, _} = cowboyku_req:header(vegur_utils:config(start_time_header), Req),
            true = is_integer(list_to_integer(binary_to_list(Res)))
    after 5000 ->
            throw(timeout)
    end,
    Config.

connect_time_header(Config) ->
    Url = url(Config),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {Res, _} = cowboyku_req:header(vegur_utils:config(connect_time_header), Req),
            true = is_integer(list_to_integer(binary_to_list(Res)))
    after 5000 ->
            throw(timeout)
    end,
    Config.

route_time_header(Config) ->
    Url = url(Config),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {Res, _} = cowboyku_req:header(vegur_utils:config(route_time_header), Req),
            true = is_integer(list_to_integer(binary_to_list(Res)))
    after 5000 ->
            throw(timeout)
    end,
    Config.

service_try_again(Config) ->
    Url = url(Config),
    meck:expect(vegur_stub, service_backend,
                fun(_, Req, HandlerState) ->
                        mock_backend(?config(backend_port, Config)),
                        {{{127,0,0,1}, ?config(backend_port, Config)+1}, Req, HandlerState}
                end),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {Res, _} = cowboyku_req:header(vegur_utils:config(connect_time_header), Req),
            true = is_integer(list_to_integer(binary_to_list(Res)))
    after 5000 ->
            throw(timeout)
    end,
    Config.

request_statistics(Config) ->
    Url = url(Config) ++ "/?abc=d",
    mock_terminate(self()),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, _Req} ->
            receive
                {stats, {successful, Upstream, _State}} ->
                    ct:pal("Upstream: ~p~n",[Upstream]),
                    {118, _} = vegur_req:bytes_recv(Upstream),
                    case ?config(scheme, Config) of
                        "http" ->
                            {273, _} = vegur_req:bytes_sent(Upstream);
                        "https" ->
                            %% The total payload is 2 bytes longer for
                            %% https requests. One byte is the "s" in
                            %% "https", the other is the 3 digit port
                            %% number "443" instead of a two digit
                            %% port number "80"
                            {275, _} = vegur_req:bytes_sent(Upstream)
                        end,
                    {RT, _} = vegur_req:route_duration(Upstream),
                    {CT, _} = vegur_req:connect_duration(Upstream),
                    {TT, _} = vegur_req:total_duration(Upstream),
                    {SH, _} = vegur_req:send_headers_duration(Upstream),
                    {QP, _} = vegur_req:request_proxy_duration(Upstream),
                    {RP, _} = vegur_req:response_proxy_duration(Upstream),
                    {[], _} = vegur_req:connection_info(Upstream),
                    {[], _} = vegur_req:connection_info([protocol], Upstream),
                    {[], _} = vegur_req:connection_info([cipher_suite], Upstream),
                    {[], _} = vegur_req:connection_info([sni_hostname], Upstream),
                    {[], _} = vegur_req:connection_info([protocol, sni_hostname], Upstream),
                    {<<"/?abc=d">>, _} = vegur_req:raw_path(Upstream),
                    true = lists:all(fun(X) -> is_integer(X) end,
                                     [RT, CT, TT, SH, QP, RP])
            after 5000 ->
                    throw(timeout)
            end
    after 5000 ->
            throw(timeout)
    end,
    Config.

request_keepalive_statistics(Config) ->
    Url = url(Config) ++ "/?abc=d",
    mock_terminate(self()),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive {req, _} -> ok after 5000 -> throw(timeout) end,
    SchemeSize = case ?config(scheme, Config) of
        "http" ->
            278;
        "https" ->
            %% The total payload is 2 bytes longer for
            %% https requests. One byte is the "s" in
            %% "https", the other is the 3 digit port
            %% number "443" instead of a two digit
            %% port number "80"
            280
    end,
    Stats1 = receive
        {stats, {successful, Upstream1, _}} ->
            {123, _} = vegur_req:bytes_recv(Upstream1),
            {SchemeSize, _} = vegur_req:bytes_sent(Upstream1),
            {[], _} = vegur_req:connection_info(Upstream1),
            {[], _} = vegur_req:connection_info([protocol], Upstream1),
            {[], _} = vegur_req:connection_info([cipher_suite], Upstream1),
            {[], _} = vegur_req:connection_info([sni_hostname], Upstream1),
            {[], _} = vegur_req:connection_info([protocol, sni_hostname], Upstream1),
            [X || {X, _} <- [
             {_, _} = vegur_req:route_duration(Upstream1),
             {_, _} = vegur_req:total_duration(Upstream1),
             {_, _} = vegur_req:send_headers_duration(Upstream1),
             {_, _} = vegur_req:request_proxy_duration(Upstream1),
             {_, _} = vegur_req:response_proxy_duration(Upstream1)
            ]]
    after 5000 ->
        throw(timeout)
    end,
    %% Second Req
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive {req, _} -> ok after 5000 -> throw(timeout) end,
    Stats2 = receive
        {stats, {successful, Upstream2, _}} ->
            {123, _} = vegur_req:bytes_recv(Upstream2),
            {SchemeSize, _} = vegur_req:bytes_sent(Upstream2),
            {0, _} = vegur_req:connect_duration(Upstream2), % keepalive got no connect
            {[], _} = vegur_req:connection_info(Upstream2),
            {[], _} = vegur_req:connection_info([protocol], Upstream2),
            {[], _} = vegur_req:connection_info([cipher_suite], Upstream2),
            {[], _} = vegur_req:connection_info([sni_hostname], Upstream2),
            {[], _} = vegur_req:connection_info([protocol, sni_hostname], Upstream2),
            [X || {X, _} <- [
             {_, _} = vegur_req:route_duration(Upstream2),
             {_, _} = vegur_req:total_duration(Upstream2),
             {_, _} = vegur_req:send_headers_duration(Upstream2),
             {_, _} = vegur_req:request_proxy_duration(Upstream2),
             {_, _} = vegur_req:response_proxy_duration(Upstream2)
            ]]
    after 5000 ->
        throw(timeout)
    end,
    ComparativeStats = lists:zip(Stats1, Stats2),
    ct:pal("comparative stats: ~p", [ComparativeStats]),
    %% make sure some of them are different across runs, and not always
    %% increasing. This is sadly not a deterministic test, but ought to be
    %% good enough.
    ?assert(lists:any(fun({X,Y}) -> X =/= Y end, ComparativeStats)),
    ?assert(lists:any(fun({X,Y}) -> X >= Y end, ComparativeStats)),
    ?assert(lists:all(fun({X,Y}) -> is_integer(X) andalso is_integer(Y) end,
                      ComparativeStats)),
    Config.


response_attributes(Config) ->
    %% Test vegur_req attributes that have to do with the response
    %% from the back-end to the client. These so far include the
    %% status code and the HTTP headers.
    Url = url(Config),
    mock_terminate(self()),
    {ok, {{_, 200, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, _Req} ->
            receive
                {stats, {successful, Upstream, _State}} ->
                    ct:pal("Upstream: ~p~n",[Upstream]),
                    {200, _} = vegur_req:response_code(Upstream),
                    {Headers, _} = vegur_req:response_headers(Upstream),
                    true = lists:member({<<"remote-host">>,<<"localhost">>}, Headers),
                    true = lists:member({<<"set-cookie">>,<<"my=cookie;">>}, Headers),
                    true = lists:member({<<"x-tst">>,<<"1337">>}, Headers)
            after 5000 ->
                    throw(timeout)
            end
    after 5000 ->
            throw(timeout)
    end,
    Config.

websockets(Config) ->
    Msg = <<"Test Msg!">>,
    Port = ?config(vegur_port, Config),
    Host = "127.0.0.1",
    Scheme = case ?config(scheme, Config) of
                 "http" -> "ws";
                 "https" -> "wss"
             end,
    Url = Scheme++"://"++Host++":"++integer_to_list(Port)++"/ws",
    {ok, Pid} = vegur_websocket_client:start_link(Url, self(), Msg),
    receive
        {msg, Msg} ->
            ok
    after
        5000 -> throw(timeout)
    end,
    Pid ! stop,
    Config.

host(Config) ->
    Url = url(Config),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {<<"localhost">>, _} = cowboyku_req:header(<<"host">>, Req)
    after 5000 ->
            throw(timeout)
    end,
    Config.

query_string(Config) ->
    Url = url(Config) ++ "?test=foo&bar=car#fragment=ding",
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            ?assertMatch({<<"test=foo&bar=car">>, _}, cowboyku_req:qs(Req)),
            {<<"test=foo&bar=car">>, _} = cowboyku_req:qs(Req)
    after 5000 ->
            throw(timeout)
    end,
    Config.

content_length(Config) ->
    Port = ?config(vegur_port, Config),
    Body = <<"This is a test.">>,
    Length = iolist_to_binary(integer_to_list(byte_size(Body))),
    Transport = case ?config(scheme, Config) of
                    "http" -> gen_tcp;
                    "https" -> ssl
                end,
    {ok, S} = Transport:connect({127,0,0,1}, Port, []),
    Transport:send(S,[<<"GET / HTTP/1.1\r\n"
                        "Content-Type: text/plain\r\n"
                        "Host: localhost\r\n"
                        "Content-Length: ", Length/binary, "\r\n"
                        "\r\n",
                        Body/binary>>]),
    receive
        {req, Req} ->
            {Length, _} = cowboyku_req:header(<<"content-length">>, Req)
    after 5000 ->
            throw(timeout)
    end,
    Config.

no_content_length(Config) ->
    Url = url(Config),
    {ok, {{_, 204, _}, _, _}} =
        httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {undefined, _} = cowboyku_req:header(<<"content-length">>, Req)
    after 5000 ->
            throw(timeout)
    end,
    Config.

custom_downstream_headers(Config) ->
    %% Pass a tuple that represents the handler state to enable router_metrics.
    %% We don't have access to the record definition in the tests so do it manually
    meck:expect(vegur_stub, additional_headers,
                fun(upstream, Log, Req, _HandlerState) ->
                    meck:passthrough([upstream, Log, Req, {state, 0, [router_metrics]}]);
                   (downstream, _, _, HandlerState) ->
                    {[{<<"custom-header">>,<<"custom-value">>}], HandlerState}
                end),
    Url = url(Config),
    mock_terminate(self()),
    {ok, {{_, 204, _}, Headers, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    {"custom-header", "custom-value"} = lists:keyfind("custom-header", 1, Headers),
    Config.


%% Helpers
start_backend(Port, Opts) ->
    {ok, Pid} = vegur_backend:start(Port, Opts),
    Pid.

stop_backend() ->
    vegur_backend:stop().

mock_through() ->
    meck:expect(vegur_stub, lookup_domain_name,
                fun(_, Upstream, HandlerState) ->
                        {ok, test_domain, Upstream, HandlerState}
                end),
    meck:expect(vegur_stub, checkout_service,
                fun(_, Upstream, HandlerState) ->
                        {service, test_route, Upstream, HandlerState}
                end).

mock_backend(Port) ->
    meck:expect(vegur_stub, service_backend,
                fun(_, Upstream, HandlerState) ->
                        {{{127,0,0,1}, Port}, Upstream, HandlerState}
                end).

mock_keepalive_backend(Port) ->
    meck:expect(vegur_stub, service_backend,
                fun(_, Upstream, HandlerState) ->
                        {{keepalive, {default, {{127,0,0,1}, Port}}}, Upstream, HandlerState}
                end).

mock_terminate(Test) ->
    meck:expect(vegur_stub, terminate,
                fun(Status, Upstream, HandlerState) ->
                        Test ! {stats, {Status, Upstream, HandlerState}}
                end).
