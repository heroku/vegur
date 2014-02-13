-module(vegur_continue_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    case vegur_utils:parse_header(<<"expect">>, Req) of
        {[<<"100-continue">>], Req1} ->
            %% We only have continue as an expectation
            {Req2, Env2} = handle_feature(Req1, Env),
            {ok, Req2, Env2};
        {[undefined], Req1} ->
            %% no Expect header
            {ok, Req1, Env};
        _ ->
            %% Any other value for expect headers
            {HttpCode, Req1} = vegur_utils:handle_error(expectation_failed, Req),
            {error, HttpCode, Req1}
    end.

handle_feature(Req, Env) ->
    {Features, Req2} = vegur_features:get_features(Req),
    case vegur_features:lookup(deep_continue, Features) of
        enabled ->
            {cowboy_req:set_meta(continue, continue, Req2), Env};
        disabled ->
            Headers = cowboy_req:get(headers, Req2),
            NewHeaders = vegur_utils:delete_all_headers(<<"expect">>, Headers),
            Req3 = cowboy_req:set([{headers, NewHeaders}], Req2),
            {{Transport, Socket}, _Discard} = cowboy_req:raw_socket(Req3),
            Transport:send(Socket, <<"HTTP/1.1 100 Continue\r\n\r\n">>),
            {Req3, Env}
    end.
