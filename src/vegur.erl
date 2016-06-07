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
%%%-------------------------------------------------------------------
%% @copyright Heroku (2013-2015)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @author Omar Yasin <omarkj@heroku.com>
%% @version {@vsn}, {@date} {@time}
%% @doc vegur public API
%% @end
%%%-------------------------------------------------------------------

-module(vegur).
-define(HTTP_REF, vegur_http).
-define(HTTPS_REF, vegur_https).
-define(PROXY_REF, vegur_proxy).

%% API
-export([start_http/3
        ,start_https/4
        ,start_proxy/3
        ,stop_http/0
        ,stop_https/0
        ,stop_proxy/0
        ,stop_http/1
        ,stop_proxy/1
        ,defaults/0
        ]).

-export([default_middlewares/0]).

-type ms() :: non_neg_integer().
-type middleware() :: module().
-type options() :: [option()]|[].
-type option() :: {ref, atom()}|
                  {max_request_line_length, non_neg_integer()}|
                  {max_header_name_length, non_neg_integer()}|
                  {max_header_value_length, non_neg_integer()}|
                  {max_headers, non_neg_integer()}|
                  {timeout, ms()}|
                  {middlewares, [middleware()]}|
                  {request_id_header, binary()}|
                  {connect_time_header, binary()}|
                  {route_time_header, binary()}|
                  {request_id_max_size, non_neg_integer()}|
                  {downstream_connect_timeout, ms()}.


-spec start_http(PortNumber, Interface, Options) ->
                        {ok, pid()}|no_return() when
      PortNumber :: inet:port_number(),
      Interface :: module(),
      Options :: options().
start_http(Port, Interface, Config) ->
    HttpRef = proplists:get_value(ref, Config, ?HTTP_REF),
    start(http, HttpRef, Port, Interface, Config, []).

-spec start_https(PortNumber, Interface, Options, SSLOptions) ->
                        {ok, pid()}|no_return() when
      PortNumber :: inet:port_number(),
      Interface :: module(),
      Options :: options(),
      SSLOptions :: [ssl:ssloption()].
start_https(Port, Interface, Config, SSLOptions) ->
    HttpsRef = proplists:get_value(ref, Config, ?HTTPS_REF),
    start(https, HttpsRef, Port, Interface,
          Config, SSLOptions).

-spec start_proxy(PortNumber, Interface, Options) ->
                         {ok, pid()}|no_return() when
      PortNumber :: inet:port_number(),
      Interface :: module(),
      Options :: options().
start_proxy(Port, Interface, Config) ->
    ProxyRef = proplists:get_value(ref, Config, ?PROXY_REF),
    start(proxy, ProxyRef, Port, Interface, Config, []).

-spec stop_http() -> ok.
stop_http() ->
    stop_http(?HTTP_REF).

-spec stop_https() -> ok.
stop_https() ->
    stop_http(?HTTPS_REF).

-spec stop_proxy() -> ok.
stop_proxy() ->
    stop_proxy(?PROXY_REF).

-spec stop_http(atom()) -> ok.
stop_http(Ref) ->
    cowboyku:stop_listener(Ref).

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

-spec defaults() -> [proplists:property()].
defaults() ->
    [{middlewares, [vegur_midjan_middleware]}
    ,{max_request_line_length, 8192}
    ,{max_header_name_length, 1000}
    ,{max_header_value_length, 8192}
    ,{max_headers, 1000}
    ,{timeout, timer:seconds(60)}
    ,{onrequest, fun vegur_request_log:new/1}
    ,{onresponse, fun vegur_request_log:done/4}
    ].

%% Internal
-spec start(Type, Ref, Port, Interface, Config, SocketOptions)
           -> {ok, pid()} | {error, badarg} when
      Type :: http | https | proxy,
      Ref :: atom(),
      Port :: inet:port_number(),
      Interface :: module(),
      Config :: proplists:proplists(),
      SocketOptions :: proplists:proplists().
start(Type, Ref, Port, Interface, Config, SocketOptions) ->
    { Acceptors,
      MaxConnections,
      NewConfig } = prestart_config(Config),
    ok = set_config(interface_module, Interface),
    start_listener(Type, Ref, Port, Acceptors,
                   MaxConnections, NewConfig, SocketOptions).

-spec prestart_config(Config :: proplists:proplists()) ->
                             { Acceptors :: pos_integer(),
                               MaxConnections :: pos_integer(),
                               NewConfig :: proplists:proplists() }.
prestart_config(Config) ->
    lists:foldl(
      fun({ConfigName, AppConfigName, Default},
          {Acceptors, MaxConnections, ConfigIn}) ->
              {Value, ConfigOut} = get_default(ConfigName, ConfigIn, Default),
              ok = set_config(AppConfigName, Value),
              {Acceptors, MaxConnections, ConfigOut};
         (acceptors, {_, MaxConnections, ConfigIn}) ->
              {Acceptors, ConfigOut} =
                  get_default(acceptors,
                              ConfigIn,
                              vegur_utils:config(acceptors)),
              {Acceptors, MaxConnections, ConfigOut};
         (max_connections, {Acceptors, _, ConfigIn}) ->
              {MaxConnections, ConfigOut} =
                  get_default(max_connections,
                              ConfigIn,
                              vegur_utils:config(max_connections)),
              {Acceptors, MaxConnections, ConfigOut}
      end,
      %% Defaults
      {vegur_utils:config(acceptors), vegur_utils:config(max_connections), Config},
      [
       {middlewares, middleware_stack, default_middlewares()},
       {request_id_header, request_id_name,
        vegur_utils:config(request_id_name)},
       {connect_time_header, connect_time_header,
        vegur_utils:config(connect_time_header)},
       {route_time_header, route_time_header,
        vegur_utils:config(route_time_header)},
       {request_id_max_size, request_id_max_size,
        vegur_utils:config(request_id_max_size)},
       {downstream_connect_timeout,
        downstream_connect_timeout,
        vegur_utils:config(downstream_connect_timeout)},
       acceptors,
       max_connections
      ]).

-spec start_listener(Type, Ref, Port, Acceptors,
                     MaxConnections, Config, SocketOptions) ->
                            {ok, pid()} | {error, badarg} when
      Type :: http | https | proxy,
      Ref :: atom(),
      Port :: inet:port_number(),
      Acceptors :: pos_integer(),
      MaxConnections :: pos_integer(),
      Config :: proplists:proplists(),
      SocketOptions :: proplists:proplists().
start_listener(http, Ref, Port, Acceptors,
               MaxConnections, Config, SocketOptions) ->
    cowboyku:start_http(Ref, Acceptors,
                        [{port, Port},
                         {max_connections, MaxConnections}
                         | SocketOptions ] ++ extra_socket_options(),
                        merge_options(defaults(), Config));
start_listener(https, Ref, Port, Acceptors,
               MaxConnections, Config, SocketOptions) ->
    cowboyku:start_https(Ref, Acceptors,
                        [{port, Port},
                         {max_connections, MaxConnections}
                         | SocketOptions ] ++ extra_socket_options(),
                        merge_options(defaults(), Config));
start_listener(proxy, Ref, Port, Acceptors,
               MaxConnections, Config, SocketOptions) ->
    ranch:start_listener(Ref, Acceptors,
                         ranch_proxy,
                         [{port, Port},
                          {max_connections, MaxConnections}
                         | SocketOptions ] ++ extra_socket_options(),
                         cowboyku_protocol,
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

extra_socket_options() ->
    vegur_utils:config(extra_socket_options, []).
