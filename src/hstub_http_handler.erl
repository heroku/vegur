%% @copyright Heroku (2013)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Default HTTP handler for hstub.
%% @end
-module(hstub_http_handler).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-include("include/hstub_log.hrl").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200,
                                  [{<<"Content-Type">>, <<"text/plain">>}],
                                  <<"Hello world!">>,
                                  Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
