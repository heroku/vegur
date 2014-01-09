-module(vegur_midjan_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

-spec execute(Req, Env) ->
                     {ok, Req, Env}|
                     {halt, Req}|
                     {error, StatusCode, Req} when
      Req :: cowboy_req:req(),
      Env :: cowboy_middleware:env(),
      StatusCode :: cowboy:http_status().
execute(Req, Env) ->
    case midjan_core:start({Req, Env}, [{ordered, vegur_app:middleware_stack()},
                                        {translator, vegur_midjan_translator}
                                       ]) of
        {done, {halt, _Req} = Ret} ->
            Ret;
        {done, {error, _StatusCode, _Req} = Ret} ->
            Ret;
        {done, {Req1, Env1}} ->
            {ok, Req1, Env1}
    end.
