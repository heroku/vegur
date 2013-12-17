-module(hstub_continue_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    case hstub_utils:parse_header(<<"expect">>, Req) of
        {[<<"100-continue">>], Req1} ->
            %% We only have continue as an expectation
            {Type, Req2} = cowboy_req:meta(request_type, Req1, []),
            Req3 = cowboy_req:set_meta(request_type, [continue|Type], Req2),
            {ok, Req3, Env};
        {[undefined], Req1} ->
            %% no Expect header
            {ok, Req1, Env};
        _ ->
            %% Any other value for expect headers
            {error, 417, Req}
    end.
