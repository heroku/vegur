%%%-------------------------------------------------------------------
%% @copyright Heroku (2013)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc hstub OTP application callback module.
%% @end
%%%-------------------------------------------------------------------

-module(hstub_app).

-behaviour(application).

-define(APP, hstub).

%% Application callbacks
-export([start_phase/3, start/2, stop/1]).

-export([config/0, config/1, config/2,
         start/0]).

%%%===================================================================
%%% Convenience Functions
%%%===================================================================

start() ->
    application:ensure_all_started(?APP, permanent).

config(Key, Default) ->
    case application:get_env(?APP, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

config(Key) ->
    case application:get_env(?APP, Key) of
        undefined -> erlang:error({missing_config, Key});
        {ok, Val} -> Val
    end.

config() ->
    application:get_all_env(?APP).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    stillir:set_config(?APP, env_specs()),
    hstub_sup:start_link().

stop(_State) ->
    ok.

start_phase(listen, _Type, _Args) ->
    {ok, _} = cowboy:start_http(?APP, config(http_acceptors),
                                [{port, config(http_listen_port)}],
                                [{env, []},
                                 {onrequest, fun hstub_log_hook:on_request/1},
                                 {middlewares, [hstub_healthcheck_middleware,
                                                hstub_validate_headers,
                                                hstub_lookup_domain_middleware,
                                                hstub_maintenance_middleware,
                                                hstub_upgrade_middleware,
                                                hstub_lookup_service_middleware,
                                                hstub_proxy_middleware
                                               ]}
                                ]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

env_specs() ->
    [{http_listen_port, "PORT", [{transform, integer}]}
     ].
