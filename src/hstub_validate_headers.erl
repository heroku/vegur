-module(hstub_validate_headers).
-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    % Check for empty host
    {Host, Req1} = cowboy_req:host(Req),
    validate_host(Host, Req1, Env).
    %{ok, Req, Env}.

validate_host(<<>>, Req, _Env) ->
    % The Host header is empty, return 400
    {error, 400, Req};
validate_host(_Host, Req, Env) ->
    % Check for a Expect header. If it is set, it needs to be set to 100-continue
    case cowboy_req:header(<<"expect">>, Req) of
        {undefined, Req1} ->
            {ok, Req1, Env};
        {<<"100-continue">>, Req1} ->
            {ok, Req1, Env};
        {_, Req1} ->
            {error, 417, Req1}
    end.
