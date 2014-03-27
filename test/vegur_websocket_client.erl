-module(vegur_websocket_client).

-behaviour(websocket_client_handler).

-include_lib("common_test/include/ct.hrl").

-export([
         start_link/3,
         init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

-record(state, {pid, msg}).

start_link(Url, Pid, Msg) ->
    websocket_client:start_link(Url,
                               ?MODULE, [Pid, Msg]).

init([Pid, Msg], _C) ->
    websocket_client:cast(self(), {text, Msg}),
    {ok, #state{pid=Pid, msg=Msg}}.

websocket_handle({text, Msg}, _, State=#state{pid=Pid
                                             ,msg=Msg}) ->
    Pid ! {msg, Msg},
    {ok, State}.

websocket_info(_, _, State) ->
    {ok, State}.

websocket_terminate(_, _ConnState, _State) ->
    ok.
