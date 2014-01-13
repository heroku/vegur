-module(vegur_proxy_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, vegur_proxy_headers}
    ].

groups() ->
    [
     {vegur_proxy_headers, [], [request_id
                                ,forwarded_for
                                ,via
                                ,connect_time_header
                                ,route_time_header
                               ]}
    ].

init_per_suite(Config) ->
    {ok, Cowboy} = application:ensure_all_started(cowboy),
    {ok, Inets} = application:ensure_all_started(inets),
    {ok, Meck} = application:ensure_all_started(meck),
    application:load(vegur),
    [{started, Cowboy++Inets++Meck}| Config].

end_per_suite(Config) ->
    [application:stop(App) || App <- ?config(started, Config)],
    Config.

init_per_group(vegur_proxy_headers, Config) ->
    VegurPort = 9333,
    DynoPort = 9555,
    mock_through(DynoPort),
    application:set_env(vegur, http_listen_port, VegurPort),
    {ok, VegurStarted} = application:ensure_all_started(vegur),
    [{group_started, VegurStarted},
     {vegur_port, VegurPort},
     {dyno_port, DynoPort} | Config].

end_per_group(vegur_proxy_headers, Config) ->
    meck:unload(),
    [application:stop(App) || App <- ?config(group_started, Config)],
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
            {RequestId, _} = cowboy_req:header(vegur_app:config(request_id_name), Req),
            valid = erequest_id:validate(RequestId, vegur_app:config(request_id_max_size))
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {binary_to_list(vegur_app:config(request_id_name)),
                                                           "testid"}]}, [], []),
    receive
        {req, Req1} ->
            {<<"testid">>, _} = cowboy_req:header(vegur_app:config(request_id_name), Req1)
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {binary_to_list(vegur_app:config(request_id_name)),
                                                           "testid??"}]}, [], []),
    receive
        {req, Req2} ->
            {RequestId1, _} = cowboy_req:header(vegur_app:config(request_id_name), Req2),
            true = RequestId1 /= <<"testid??">>,
            valid = erequest_id:validate(RequestId1, vegur_app:config(request_id_max_size))
    after 5000 ->
            throw(timeout)
    end,
    Config.

forwarded_for(Config) ->
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost:"++integer_to_list(Port)}]}, [], []),
    receive
        {req, Req} ->
            {<<"127.0.0.1">>, _} = cowboy_req:header(<<"x-forwarded-for">>, Req),
            {DestPort, _} = cowboy_req:header(<<"x-forwarded-port">>, Req),
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
    Config.

via(Config) ->
    Port = ?config(vegur_port, Config),
    Url = "http://127.0.0.1:" ++ integer_to_list(Port),
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"}]}, [], []),
    receive
        {req, Req} ->
            {<<"vegur">>, _} = cowboy_req:header(<<"via">>, Req)
    after 5000 ->
            throw(timeout)
    end,
    {ok, {{_, 204, _}, _, _}} = httpc:request(get, {Url, [{"host", "localhost"},
                                                          {"via", "happyproxy"}]}, [], []),
    receive
        {req, Req1} ->
            {<<"happyproxy, vegur">>, _} = cowboy_req:header(<<"via">>, Req1)
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
            {Res, _} = cowboy_req:header(vegur_app:config(connect_time_header), Req),
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
            {Res, _} = cowboy_req:header(vegur_app:config(route_time_header), Req),
            true = is_integer(list_to_integer(binary_to_list(Res)))
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

mock_through(Port) ->
    meck:new(vegur_stub, [no_link, passthrough]),
    meck:expect(vegur_stub, lookup_domain_name,
                fun(_, HandlerState) ->
                        {ok, test_domain, HandlerState}
                end),
    meck:expect(vegur_stub, checkout_service,
                fun(_, HandlerState) ->
                        {service, test_route, HandlerState}
                end),
    meck:expect(vegur_stub, service_backend,
                fun(_, HandlerState) ->
                        {{{127,0,0,1}, Port}, HandlerState}
                end).
