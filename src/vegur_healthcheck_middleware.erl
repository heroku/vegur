-module(vegur_healthcheck_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    % Check if this is a healthcheck request
    {Path, Req1} = cowboy_req:path(Req),
    maybe_healthcheck(Path, Req1, Env).

-spec maybe_healthcheck(binary(), Req, Env) ->
                               {error, ErrorCode, Req} |
                               {halt, Req} |
                               {ok, Req, Env} when
      Req :: cowboy_req:req(),
      Env :: cowboy_middleware:env(),
      ErrorCode :: 500.
maybe_healthcheck(<<"/F3DA8257-B28C-49DF-AACD-8171464E1D1D">>, Req, _Env) ->
    % This is an upstream proxy healthcheck, check if this node is being drained and reply
    case vegur_healthchecks:accepting_connections() of
        true ->
            Req1 = set_response_code(200, Req),
            Req2 = vegur_utils:set_request_status(healthcheck, Req1),
            {halt, 200, Req2};
        _ ->
            Req1 = vegur_utils:set_request_status(healthcheck_error, Req),
            {error, 500, Req1}
    end;
maybe_healthcheck(<<"/lockstep">>, Req, Env) ->
    case cowboy_req:host(Req) of
        {<<"hermes.localhost">>, Req1} ->
            case vegur_healthchecks:lockstep_fresh() of
                true ->
                    Req2 = set_response_code(200, Req1),
                    Req3 = vegur_utils:set_request_status(healthcheck, Req2),
                    {halt, 200, Req3};
                _ ->
                    Req2 = vegur_utils:set_request_status(healthcheck_error, Req1),
                    {error, 500, Req2}
            end;
        {_, Req1} ->
            {ok, Req1, Env}
    end;
maybe_healthcheck(<<"/healthcheck">>, Req, Env) ->
    HerokuappDomain = vegur_utils:config(herokuapp_domain),
    case cowboy_req:host(Req) of
        {<<"hermes.", HerokuappDomain/binary>>, Req1} ->
            Req2 = set_response_code(200, Req1),
            Req3 = vegur_utils:set_request_status(healthcheck, Req2),
            {halt, 200, Req3};
        {_, Req1} ->
            {ok, Req1, Env}
    end;
maybe_healthcheck(_, Req, Env) ->
    {ok, Req, Env}.

set_response_code(Code, Req) ->
    {ok, Req1} = cowboy_req:reply(Code, [], Req),
    Req1.
