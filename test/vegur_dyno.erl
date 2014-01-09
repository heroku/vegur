-module(vegur_dyno).
-behaviour(cowboy_http_handler).

-export([start/2, stop/0]).
-export([init/3, handle/2, terminate/3]).

start(Port, Opts) ->
    Dispatch = cowboy_router:compile([
            {'_', [{'_', ?MODULE, Opts}]}
        ]),
    cowboy:start_http(?MODULE, 100,
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]).

stop() ->
    cowboy:stop_listener(?MODULE).

init(_Transport, Req, Opts) ->
    {ok, Req, Opts}.

handle(Req, Opts) ->
    F = proplists:get_value(in_handle, Opts, fun(R) -> R end),
    F(Req),
    {ok, Req, Opts}.

terminate(_Reason, _Req, _State) ->
    ok.
