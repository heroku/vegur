-module(vegur_lookup_service_middleware).

-behaviour(cowboy_middleware).
-include("vegur_log.hrl").
-export([execute/2]).

execute(Req, Env) ->
    lookup_service(Req, Env, undefined).

lookup_service(Req, Env, LookupStats) ->
    InterfaceModule = vegur_utils:get_interface_module(Env),
    {DomainGroup, Req1} = cowboy_req:meta(domain_group, Req),
    {Service, Req2} = ?LOG(service_lookup, InterfaceModule:checkout_service(DomainGroup, LookupStats),
                           Req1),
    handle_service(Service, Req2, Env).

handle_service({service, Service, LookupStats}, Req, Env) ->
    InterfaceModule = vegur_utils:get_interface_module(Env),
    ServiceBackend = InterfaceModule:service_backend(Service),
    Req1 = vegur_request_log:stamp(pre_connect, Req),
    case ?LOG(connect_time, vegur_proxy:backend_connection(ServiceBackend), Req1) of
        {{connected, Client}, Req2} ->
            Req3 = cowboy_req:set_meta(backend_connection, Client, Req2),
            {ok, Req3, Env};
        {{error, Reason}, Req2} ->
            {DomainGroup, Req3} = cowboy_req:meta(domain_group, Req2),
            {ok, LookupStats1} = InterfaceModule:checkin_service(DomainGroup, Service, Reason, LookupStats),
            lookup_service(Req3, Env, LookupStats1)
    end;
handle_service({error, Reason, _LookupStats}, Req, Env) ->
    InterfaceModule = vegur_utils:get_interface_module(Env),
    {DomainGroup, Req1} = cowboy_req:meta(domain_group, Req),
    {HttpCode, ErrorHeaders, ErrorBody} = InterfaceModule:error_page(Reason, DomainGroup),
    Req2 = vegur_utils:render_response(HttpCode, ErrorHeaders, ErrorBody, Req1),
    {halt, Req2}.
