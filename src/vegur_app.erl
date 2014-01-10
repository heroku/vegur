%%%-------------------------------------------------------------------
%% @copyright Heroku (2013)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc vegur OTP application callback module.
%% @end
%%%-------------------------------------------------------------------

-module(vegur_app).

-behaviour(application).

-define(APP, vegur).
-define(HTTP_REF, vegur_http).
-define(PROXY_REF, vegur_proxy).

%% Application callbacks
-export([start_phase/3, start/2, stop/1]).

-export([config/0, config/1, config/2,
         start/0]).

-export([middleware_stack/0]).

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

middleware_stack() ->
    [vegur_healthcheck_middleware
    ,vegur_validate_headers
    ,vegur_continue_middleware
    ,vegur_lookup_domain_middleware
    ,vegur_upgrade_middleware
    ,vegur_lookup_service_middleware
    ,vegur_proxy_middleware
    ].

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    vegur_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(?PROXY_REF),
    cowboy:stop_listener(?HTTP_REF),
    ok.

start_phase(listen, _Type, _Args) ->
    {ok, _} = cowboy:start_http(?HTTP_REF, config(http_acceptors),
                                [{port, config(http_listen_port)}],
                                cowboy_opts()),
    {ok, _} = ranch:start_listener(?PROXY_REF, config(proxy_acceptors),
                                   ranch_proxy,
                                   [{port, config(proxy_listen_port)}],
                                   cowboy_protocol,
                                   cowboy_opts()),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cowboy_opts() ->
    [{env, cowboy_env()}
    ,{middlewares, [vegur_midjan_middleware]}
    ,{onrequest, fun vegur_request_log:new/1}].

cowboy_env() ->
    InterfaceModule = config(interface_module),
    [{interface_module, InterfaceModule}].
