-module(vegur_midjan_middleware).
-behaviour(cowboyku_middleware).

-export([execute/2]).

-spec execute(Req, Env) ->
                     {ok, Req, Env}|
                     {halt, Req}|
                     {error, StatusCode, Req} when
      Req :: cowboyku_req:req(),
      Env :: cowboyku_middleware:env(),
      StatusCode :: cowboyku:http_status().
execute(Req, Env) ->
    case midjan_core:start({Req, Env}, [{ordered, vegur_utils:config(middleware_stack)},
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
        {halt, _Code, Req0} -> Req0;
        {error, _Code, Req0} -> Req0
    end,
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    {DomainGroup, Req2} = cowboyku_req:meta(domain_group, Req1),
    {Service, Req3} = cowboyku_req:meta(service, Req2),
    Phase = case {DomainGroup, Service} of
        {undefined, undefined} -> lookup;
        {_, undefined} -> checkout;
        _ -> connected
    end,
    {ServiceState, Req4} = cowboyku_req:meta(service_state, Req3, normal),
    {ok, Req5, HandlerState2} = InterfaceModule:checkin_service(
        DomainGroup, Service, Phase, ServiceState, Req4, HandlerState
    ),
    ReqFinal = vegur_utils:set_handler_state(HandlerState2, Req5),
    %% Call the logger
    Final = case Return of
                {halt, _} -> {halt, ReqFinal};
                {halt, Code, _} -> {halt, Code, ReqFinal};
                {error, Code, _} -> {error, Code, ReqFinal}
            end,
    vegur_request_log:done(Final).
