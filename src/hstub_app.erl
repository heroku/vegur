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
-define(HTTP_REF, hstub_http).
-define(PROXY_REF, hstub_proxy).

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
    cowboy:start_http(?HTTP_REF, config(http_acceptors),
                      [{port, config(http_listen_port)}],
                      [{env, [{handler, hstub_cc_handler}
                             ,{handler_opts, []}]}
                      ,{middlewares, [cowboy_handler]}
                      ,{onrequest, fun hstub_log_hook:on_request/1}]),
    ranch:start_listener(?PROXY_REF, config(proxy_acceptors),
                         ranch_proxy,
                         [{port, config(proxy_listen_port)}],
                         cowboy_protocol,
                         [{env, [{handler, hstub_cc_handler}
                                ,{handler_opts, []}]}
                         ,{middlewares, [cowboy_handler]}
                         ,{onrequest, fun hstub_log_hook:on_request/1}]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

env_specs() ->
    [{http_listen_port, "PORT", [{transform, integer}]}
     ].
