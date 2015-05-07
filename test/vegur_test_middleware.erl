-module(vegur_test_middleware).

-behaviour(cowboyku_middleware).
-export([execute/2]).

execute(Req, _Env) ->
    {halt, Req}.
