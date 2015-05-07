-module(vegur_continue_middleware).

-behaviour(cowboyku_middleware).
-export([execute/2]).

execute(Req, Env) ->
    case vegur_utils:parse_header(<<"expect">>, Req) of
        {ok, {[<<"100-continue">>], Req1}} ->
            %% We only have continue as an expectation
            {Req2, Env2} = handle_feature(Req1, Env),
            {ok, Req2, Env2};
        {ok, {[undefined], Req1}} ->
            %% no Expect header
            {ok, Req1, Env};
        {ok, _} ->
            %% Any other value for expect headers
            {HttpCode, Req1} = vegur_utils:handle_error(expectation_failed, Req),
            {error, HttpCode, Req1};
        {error, _} ->
            %% Bad request, invalid header value
            {HttpCode, Req1} = vegur_utils:handle_error(bad_request_header, Req),
            {error, HttpCode, Req1}
    end.

handle_feature(Req, Env) ->
    {InterfaceModule, HandlerState, Req2} = vegur_utils:get_interface_module(Req),
    case InterfaceModule:feature(deep_continue, HandlerState) of
        {enabled, HandlerState2} ->
            Req3 = vegur_utils:set_handler_state(HandlerState2, Req2),
            {cowboyku_req:set_meta(continue, continue, Req3), Env};
        {disabled, HandlerState2} ->
            Req3 = vegur_utils:set_handler_state(HandlerState2, Req2),
            Req4 = flush_expect_headers(Req3),
            send_continue(Req4),
            {Req4, Env}
    end.

flush_expect_headers(Req) ->
    Headers = cowboyku_req:get(headers, Req),
    NewHeaders = vegur_utils:delete_all_headers(<<"expect">>, Headers),
    cowboyku_req:set([{headers, NewHeaders}], Req).

send_continue(Req) ->
    {Transport, Socket} = vegur_utils:borrow_cowboyku_socket(Req),
    Transport:send(Socket, <<"HTTP/1.1 100 Continue\r\n\r\n">>).
