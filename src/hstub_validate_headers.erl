-module(hstub_validate_headers).
-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    {Host, Req1} = cowboy_req:host(Req),
    validate_host(Host, Req1, Env).

-spec validate_host(binary(), cowboy_req:req(), any()) ->
                           {error, 400, cowboy_req:req()} |
                           {error, 417, cowboy_req:req()} |
                           {ok, cowboy_req:req(), any()}.
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
