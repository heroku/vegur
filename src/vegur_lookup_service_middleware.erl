-module(vegur_lookup_service_middleware).

-behaviour(cowboy_middleware).
-include("vegur_log.hrl").
-export([execute/2]).

execute(Req, Env) ->
    lookup_service(Req, Env).

lookup_service(Req, Env) ->
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    {DomainGroup, Req1} = cowboy_req:meta(domain_group, Req),
    Service = InterfaceModule:checkout_service(DomainGroup, HandlerState),
    handle_service(Service, Req1, Env).

handle_service({service, Service, HandlerState}, Req, Env) ->
    {InterfaceModule, _, Req1} = vegur_utils:get_interface_module(Req),
    {ServiceBackend, HandlerState1} = InterfaceModule:service_backend(Service, HandlerState),
    Req2 = vegur_request_log:stamp(pre_connect, Req1),
    case ?LOG(connect_time, vegur_proxy:backend_connection(ServiceBackend), Req2) of
        {{connected, Client}, Req3} ->
            Req4 = cowboy_req:set_meta(backend_connection, Client, Req3),
            Req5 = vegur_utils:set_handler_state(HandlerState1, Req4),
            {ok, Req5, Env};
        {{error, Reason}, Req3} ->
            {DomainGroup, Req4} = cowboy_req:meta(domain_group, Req3),
            {ok, HandlerState2} = InterfaceModule:checkin_service(DomainGroup, Service, Reason, HandlerState1),
            Req5 = vegur_utils:set_handler_state(HandlerState2, Req4),
            lookup_service(Req5, Env)
    end;
handle_service({error, Reason, HandlerState}, Req, _Env) ->
    {InterfaceModule, _, Req1} = vegur_utils:get_interface_module(Req),
    {DomainGroup, Req2} = cowboy_req:meta(domain_group, Req1),
    {{HttpCode, ErrorHeaders, ErrorBody}, HandlerState1} = InterfaceModule:error_page(Reason, DomainGroup, HandlerState),
    Req3 = vegur_utils:set_handler_state(HandlerState1, Req2),
    Req4 = vegur_utils:render_response(HttpCode, ErrorHeaders, ErrorBody, Req3),
    Req5 = vegur_utils:set_request_status(error, Req4),
    {halt, Req5}.
