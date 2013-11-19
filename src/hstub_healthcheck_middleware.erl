-module(hstub_healthcheck_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    % Check if this is a healthcheck request
    {Path, Req1} = cowboy_req:path(Req),
    maybe_healthcheck(Path, Req1, Env).

-spec maybe_healthcheck(binary(), cowboy_req:req(), any()) ->
                               {error, 500, cowboy_req:req()} |
                               {error, 200, cowboy_req:req()} |
                               {ok, cowboy_req:req(), any()}.
maybe_healthcheck(<<"/F3DA8257-B28C-49DF-AACD-8171464E1D1D">>, Req, _Env) ->
    % This is an upstream proxy healthcheck, check if this node is being drained and reply
    case hstub_app:config(proxy_deny) of
        true ->
            {error, 500, Req};
        _ ->
            {error, 200, Req}
    end;
maybe_healthcheck(<<"/healthcheck">>, Req, Env) ->
    {ok, Req, Env};
maybe_healthcheck(_, Req, Env) ->
    {ok, Req, Env}.

