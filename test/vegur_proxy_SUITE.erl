-module(vegur_proxy_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, vegur_proxy_headers}
     ,{group, vegur_proxy_connect}
    ].

groups() ->
    [
     {vegur_proxy_headers, [], [request_id
                                ,forwarded_for
                                ,via
                                ,connect_time_header
                                ,start_time_header
                                ,route_time_header
                                ,host
                                ,query_string
                               ]}
     ,{vegur_proxy_connect, [], [service_try_again
                                ,request_statistics
                                ,websockets
                                ]}
    ].

init_per_suite(Config) ->
    application:load(vegur),
    ok = application:load(websocket_client),
    meck:new(vegur_stub, [no_link, passthrough]),
    {ok, Cowboy} = application:ensure_all_started(cowboy),
    {ok, Inets} = application:ensure_all_started(inets),
    {ok, Meck} = application:ensure_all_started(meck),
    VegurPort = 9333,
    {ok, _} = vegur:start_http(VegurPort, vegur_stub, []),
    mock_through(),
    [{started, Cowboy++Inets++Meck},
     {vegur_port, VegurPort} | Config].

end_per_suite(Config) ->
    vegur:stop_http(),
    application:unload(vegur),
    application:unload(websocket_client),
    [application:stop(App) || App <- ?config(started, Config)],
    Config.

init_per_group(vegur_proxy_headers, Config) ->
    DynoPort = 9555,
    mock_backend(9555),
    [{dyno_port, DynoPort} | Config];
init_per_group(vegur_proxy_connect, Config) ->
    DynoPort = 9555,
    mock_backend(9555),
    [{dyno_port, DynoPort} | Config].

end_per_group(_, Config) ->
    meck:unload(),
    Config.

init_per_testcase(_TestCase, Config) ->
    DynoPort = ?config(dyno_port, Config),
    Self = self(),
    Dyno = start_dyno(DynoPort, [{in_handle, fun(Req) ->
                                                     Self ! {req, Req},
                                                     Req
                                             end}]),
    [{dyno, Dyno} | Config].

end_per_testcase(_TestCase, Config) ->
    stop_dyno(),
    Config.

%%%===================================================================
%%% Test cases
%%%===================================================================
request_id(Config) ->
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {RequestId, _} = cowboy_req:header(vegur_utils:config(request_id_name), Req),
            valid = erequest_id:validate(RequestId, vegur_utils:config(request_id_max_size))
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {binary_to_list(vegur_utils:config(request_id_name)),
                                                           "testid-with-a-valid-length"}]}, [], []),
    receive
        {req, Req1} ->
            {<<"testid-with-a-valid-length">>, _} = cowboy_req:header(vegur_utils:config(request_id_name), Req1)
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {binary_to_list(vegur_utils:config(request_id_name)),
                                                           "testid??"}]}, [], []),
    receive
        {req, Req2} ->
            {RequestId1, _} = cowboy_req:header(vegur_utils:config(request_id_name), Req2),
            true = RequestId1 /= <<"testid??">>,
            valid = erequest_id:validate(RequestId1, vegur_utils:config(request_id_max_size))
    after 5000 ->
            throw(timeout)
    end,
    Config.

forwarded_for(Config) ->
    meck:expect(vegur_utils, peer_ip_port,
                fun(Req) ->
                        {{PeerIp, _}, Req1} = cowboy_req:peer(Req),
                        {Port, Req2} = cowboy_req:port(Req1),
                        {{PeerIp, 1234, Port}, Req2}
                   end),
    meck:expect(vegur_stub, feature, fun(peer_port, S) ->
                                             {enabled, S};
                                        (_, S) ->
                                             {disabled, S}
                                     end),
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost:"++integer_to_list(Port)}]}, [], []),
    receive
        {req, Req} ->
            {<<"127.0.0.1">>, _} = cowboy_req:header(<<"x-forwarded-for">>, Req),
            {DestPort, _} = cowboy_req:header(<<"x-forwarded-port">>, Req),
            {<<"1234">>, _} = cowboy_req:header(<<"x-forwarded-peer-port">>, Req),
            Port = list_to_integer(binary_to_list(DestPort))
    after 5000 ->
            throw(timeout)
    end,

    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {"x-forwarded-for", "10.0.0.1"}]}, [], []),
    receive
        {req, Req1} ->
            {<<"10.0.0.1, 127.0.0.1">>, _} = cowboy_req:header(<<"x-forwarded-for">>, Req1),
            {<<"80">>, _} = cowboy_req:header(<<"x-forwarded-port">>, Req1),
            {<<"http">>, _} = cowboy_req:header(<<"x-forwarded-proto">>, Req1)
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost:443"}]}, [], []),
    receive
        {req, Req2} ->
            {<<"443">>, _} = cowboy_req:header(<<"x-forwarded-port">>, Req2),
            {<<"https">>, _} = cowboy_req:header(<<"x-forwarded-proto">>, Req2)
    after 5000 ->
            throw(timeout)
    end,

	%% Pass a tuple that represents the handler state to enable router_metrics.
	%% We don't have access to the record definition in the tests so do it manually
    meck:expect(vegur_stub, additional_headers, 
			   fun(Log, HandlerState) -> meck:passthrough([Log, {state, 0, [router_metrics]}]) end),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req3} ->
            {<<"vegur_stub">>, _} = cowboy_req:header(<<"heroku-hermes-instance-name">>, Req3),
            {ConnectTime, _} = cowboy_req:header(<<"heroku-connect-time">>, Req3),
            true = is_integer(list_to_integer(binary_to_list(ConnectTime))),
            {TotalRouteTime, _} = cowboy_req:header(<<"heroku-total-route-time">>, Req3),
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
            {undefined, _} = cowboy_req:header(<<"x-forwarded-peer-port">>, Req4)
    after 5000 ->
            throw(timeout)
    end,
    Config.

via(Config) ->
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {<<"1.1 vegur">>, _} = cowboy_req:header(<<"via">>, Req)
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {"via", "happyproxy"}]}, [], []),
    receive
        {req, Req1} ->
            {<<"happyproxy, 1.1 vegur">>, _} = cowboy_req:header(<<"via">>, Req1)
    after 5000 ->
            throw(timeout)
    end,
    Config.

start_time_header(Config) ->
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {Res, _} = cowboy_req:header(vegur_utils:config(start_time_header), Req),
            true = is_integer(list_to_integer(binary_to_list(Res)))
    after 5000 ->
            throw(timeout)
    end,
    Config.

connect_time_header(Config) ->
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {Res, _} = cowboy_req:header(vegur_utils:config(connect_time_header), Req),
            true = is_integer(list_to_integer(binary_to_list(Res)))
    after 5000 ->
            throw(timeout)
    end,
    Config.

route_time_header(Config) ->
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {Res, _} = cowboy_req:header(vegur_utils:config(route_time_header), Req),
            true = is_integer(list_to_integer(binary_to_list(Res)))
    after 5000 ->
            throw(timeout)
    end,
    Config.

service_try_again(Config) ->
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    meck:expect(vegur_stub, service_backend,
                fun(_, Req, HandlerState) ->
                        mock_backend(?config(dyno_port, Config)),
                        {{{127,0,0,1}, ?config(dyno_port, Config)+1}, Req, HandlerState}
                end),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {Res, _} = cowboy_req:header(vegur_utils:config(connect_time_header), Req),
            true = is_integer(list_to_integer(binary_to_list(Res)))
    after 5000 ->
            throw(timeout)
    end,
    Config.

request_statistics(Config) ->
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "/?abc=d",
    mock_terminate(self()),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, _Req} ->
            receive
                {stats, {successful, Upstream, _State}} ->
                    ct:pal("Upstream: ~p~n",[Upstream]),
                    {118, _} = vegur_req:bytes_recv(Upstream),
                    {292, _} = vegur_req:bytes_sent(Upstream),
                    {RT, _} = vegur_req:route_duration(Upstream),
                    {CT, _} = vegur_req:connect_duration(Upstream),
                    {TT, _} = vegur_req:total_duration(Upstream),
                    {SH, _} = vegur_req:send_headers_duration(Upstream),
                    {QP, _} = vegur_req:request_proxy_duration(Upstream),
                    {RP, _} = vegur_req:response_proxy_duration(Upstream),
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

websockets(Config) ->
    Msg = <<"Test Msg!">>,
    Port = ?config(vegur_port, Config),
    Host = "127.0.0.1",
    Url = "ws://"++Host++":"++integer_to_list(Port)++"/ws",
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
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {<<"localhost">>, _} = cowboy_req:header(<<"host">>, Req)
    after 5000 ->
            throw(timeout)
    end,
    Config.

query_string(Config) ->
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port) ++ "?test=foo&bar=car#fragment=ding",
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {<<"test=foo&bar=car">>, _} = cowboy_req:qs(Req)
    after 5000 ->
            throw(timeout)
    end,
    Config.

%% Helpers
start_dyno(Port, Opts) ->
    {ok, Pid} = vegur_dyno:start(Port, Opts),
    Pid.

stop_dyno() ->
    vegur_dyno:stop().

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

mock_terminate(Test) ->
    meck:expect(vegur_stub, terminate,
                fun(Status, Upstream, HandlerState) ->
                        Test ! {stats, {Status, Upstream, HandlerState}}
                end).
