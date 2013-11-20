-module(hstub_proxy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    {ok, Req, Env}.
