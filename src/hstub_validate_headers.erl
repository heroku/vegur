-module(hstub_validate_headers).
-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    {Host, Req1} = cowboy_req:host(Req),
    validate_host(Host, Req1, Env).

-spec validate_host(binary(), Req, Env) ->
                           {error, 400, Req} |
                           {error, 417, Req} |
                           {ok, Req, Env} when
      Req :: cowboy_req:req(),
      Env :: cowboy_middleware:env().
validate_host(<<>>, Req, _Env) ->
    % The Host header is empty, return 400
    {error, 400, Req};
validate_host(_Host, Req, Env) ->
    {ok, Req, Env}.
