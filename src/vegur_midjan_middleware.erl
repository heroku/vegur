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
                                        {translator, vegur_midjan_translator},
                                        {finally, fun finally/1}
                                       ]) of
        {done, {halt, _Req} = Ret} ->
            Ret;
        {done, {error, _StatusCode, _Req} = Ret} ->
            Ret;
        {done, {Req1, Env1}} ->
            {ok, Req1, Env1}
    end.

finally(Return) ->
    %% Check the backend back in
    Req = case Return of
        {halt, Req0} -> Req0;
        {error, _Code, Req0} -> Req0
    end,
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    {DomainGroup, Req2} = cowboy_req:meta(domain_group, Req1),
    {Service, Req3} = cowboy_req:meta(service, Req2),
    ReqFinal = case {DomainGroup, Service} of
        {undefined, undefined} -> %% Never checked out anything
            Req3;
        _ ->
            {ok, HandlerState2} = InterfaceModule:checkin_service(DomainGroup, Service, normal, HandlerState),
            vegur_utils:set_handler_state(HandlerState2, Req3)
    end,
    %% Call the logger
    Final = case Return of
        {halt, _} -> {halt, ReqFinal};
        {error, Code, _} -> {error, Code, ReqFinal}
    end,
    vegur_request_log:done(Final).
