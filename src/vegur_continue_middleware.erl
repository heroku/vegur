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
    {InterfaceModule, HandlerState, Req2} = vegur_utils:get_interface_module(Req),
    case InterfaceModule:feature(deep_continue, HandlerState) of
        {enabled, HandlerState2} ->
            Req3 = vegur_utils:set_handler_state(HandlerState2, Req2),
            {cowboy_req:set_meta(continue, continue, Req3), Env};
        {disabled, HandlerState2} ->
            Req3 = vegur_utils:set_handler_state(HandlerState2, Req2),
            Headers = cowboy_req:get(headers, Req3),
            NewHeaders = vegur_utils:delete_all_headers(<<"expect">>, Headers),
            Req4 = cowboy_req:set([{headers, NewHeaders}], Req3),
            {{Transport, Socket}, _Discard} = vegur_utils:raw_cowboy_socket(Req4),
            Transport:send(Socket, <<"HTTP/1.1 100 Continue\r\n\r\n">>),
            {Req4, Env}
    end.
