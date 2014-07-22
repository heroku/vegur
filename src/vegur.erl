%%%-------------------------------------------------------------------
%% @copyright Heroku (2013)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @author Omar Yasin <omarkj@heroku.com>
%% @version {@vsn}, {@date} {@time}
%% @doc vegur public API
%% @end
%%%-------------------------------------------------------------------

-module(vegur).
-define(HTTP_REF, vegur_http).
-define(PROXY_REF, vegur_proxy).

%% API
-export([start_http/3
         ,start_proxy/3
         ,stop_http/0
         ,stop_proxy/0
         ,stop_http/1
         ,stop_proxy/1
        ]).

-export([default_middlewares/0]).

-type ms() :: non_neg_integer().
-type middleware() :: module().
-type options() :: [option()]|[].
-type option() :: {max_request_line_length, non_neg_integer()}|
                  {max_header_name_length, non_neg_integer()}|
                  {max_header_value_length, non_neg_integer()}|
                  {max_headers, non_neg_integer()}|
                  {timeout, ms()}|
                  {middlewares, [middleware()]}|
                  {request_id_header, binary()}|
                  {connect_time_header, binary()}|
                  {route_time_header, binary()}|
                  {request_id_max_size, non_neg_integer()}.


-spec start_http(PortNumber, Interface, Options) ->
                        {ok, pid()}|no_return() when
      PortNumber :: inet:port_number(), 
      Interface :: module(),
      Options :: options().
start_http(Port, Interface, Config) ->
    HttpRef = proplists:get_value(ref, Config, ?HTTP_REF),
    start(http, HttpRef, Port, Interface, Config).

-spec start_proxy(PortNumber, Interface, Options) ->
                         {ok, pid()}|no_return() when
      PortNumber :: inet:port_number(),
      Interface :: module(),
      Options :: options().
start_proxy(Port, Interface, Config) ->
    ProxyRef = proplists:get_value(ref, Config, ?PROXY_REF),
    start(proxy, ProxyRef, Port, Interface, Config).

-spec stop_http() -> ok.
stop_http() ->
    stop_http(?HTTP_REF).

-spec stop_proxy() -> ok.
stop_proxy() ->
    stop_proxy(?PROXY_REF).

-spec stop_http(atom()) -> ok.
stop_http(Ref) ->
    cowboy:stop_listener(Ref).

-spec stop_proxy(atom()) -> ok.
stop_proxy(Ref) ->
    ranch:stop_listener(Ref).

-spec default_middlewares() -> [middleware()].
default_middlewares() ->
    [vegur_validate_headers
    ,vegur_lookup_domain_middleware
    ,vegur_continue_middleware
    ,vegur_upgrade_middleware
    ,vegur_lookup_service_middleware
    ,vegur_proxy_middleware].

-spec set_config(Key, Val) -> ok when
      Key :: atom(),
      Val :: term().
set_config(Key, Val) ->
    application:set_env(vegur, Key, Val).

%% Internal
-spec start(Type, Ref, Port, Interface, Config) -> {ok, pid()} | {error, badarg} when
    Type :: http | proxy,
    Ref :: atom(),
    Port :: inet:port_number(),
    Interface :: module(),
    Config :: proplists:proplists().
start(Type, Ref, Port, Interface, Config) ->
    {Acceptors, Config1} = get_default(acceptors, Config, vegur_utils:config(acceptors)),
    {MaxConnections, Config2} = get_default(max_connections, Config1, vegur_utils:config(max_connections)),
    {Middlewares, Config3} = get_default(middlewares, Config2, default_middlewares()),
    {RequestIdName, Config4} = get_default(request_id_header, Config3,
                                           vegur_utils:config(request_id_name)),
    {ConnectTime, Config5} = get_default(connect_time_header, Config4,
                                         vegur_utils:config(connect_time_header)),
    {RouteTimeHeader, Config6} = get_default(route_time_header, Config5,
                                             vegur_utils:config(route_time_header)),
    {RequestIdMaxSize, Config7} = get_default(request_id_max_size, Config6,
                                              vegur_utils:config(request_id_max_size)),
    {DownstreamConnectTimeout, Config8} = get_default(downstream_connect_timeout, Config7,
                                                      vegur_utils:config(downstream_connect_timeout)),
    ok = set_config(middleware_stack, Middlewares),
    ok = set_config(interface_module, Interface),
    ok = set_config(request_id_name, RequestIdName),
    ok = set_config(connect_time_header, ConnectTime),
    ok = set_config(route_time_header, RouteTimeHeader),
    ok = set_config(request_id_max_size, RequestIdMaxSize),
    ok = set_config(downstream_connect_timeout, DownstreamConnectTimeout),
    start_listener(Type, Ref, Port, Acceptors, MaxConnections, Config8).

-spec start_listener(Type, Ref, Port, Acceptors, MaxConnections, Config) ->
      {ok, pid()} | {error, badarg} when
    Type :: http | proxy,
    Ref :: atom(),
    Port :: inet:port_number(),
    Acceptors :: pos_integer(),
    MaxConnections :: pos_integer(),
    Config :: proplists:proplists().
start_listener(http, Ref, Port, Acceptors, MaxConnections, Config) ->
    cowboy:start_http(Ref, Acceptors,
                      [{port, Port},
                       {max_connections, MaxConnections}],
                      merge_options(defaults(), Config));
start_listener(proxy, Ref, Port, Acceptors, MaxConnections, Config) ->
    ranch:start_listener(Ref, Acceptors,
                         ranch_proxy,
                         [{port, Port},
                          {max_connections, MaxConnections}],
                         cowboy_protocol,
                         merge_options(defaults(), Config)).

get_default(Key, Config, Default) ->
    {proplists:get_value(Key, Config, Default),
     proplists:delete(Key, Config)}.

merge_options([], Config) ->
    Config;
merge_options([{Key, _}=DefaultPair|Rest], Config) ->
    case proplists:is_defined(Key, Config) of
        true ->
            merge_options(Rest, Config);
        false ->
            merge_options(Rest, Config++[DefaultPair])
    end.

defaults() ->
    [{middlewares, [vegur_midjan_middleware]}
     ,{max_request_line_length, 8192}
     ,{max_header_name_length, 1000}
     ,{max_header_value_length, 8192}
     ,{max_headers, 1000}
     ,{timeout, timer:seconds(60)}
     ,{onrequest, fun vegur_request_log:new/1}
     ,{onresponse, fun vegur_request_log:done/4}
     ,{request_id_name, <<"x-request-id">>}
     ,{connect_time_header, <<"connect-time">>}
     ,{route_time_header, <<"total-route-time">>}
     ,{request_id_max_size, 200}
    ].
