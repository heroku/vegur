-module(vegur_websockets_dyno).
-behaviour(cowboyku_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, Req, Opts) ->
    {upgrade, protocol, cowboyku_websocket}.

websocket_init(TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
