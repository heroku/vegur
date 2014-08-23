-module(vegur_lookup_service_middleware).

-behaviour(cowboy_middleware).
-include("vegur_log.hrl").
-export([execute/2]).

-spec execute(Req, Env) ->
                     {ok, Req, Env}|
                     {error, HttpCode, Req} when
      Req :: cowboy_req:req(),
      Env :: cowboy_middleware:env(),
      HttpCode :: cowboy:http_status().
execute(Req, Env) ->
    lookup_service(Req, Env).

lookup_service(Req, Env) ->
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    {DomainGroup, Req1} = cowboy_req:meta(domain_group, Req),
    case InterfaceModule:checkout_service(DomainGroup, Req1, HandlerState) of
        {service, Service, Req2, HandlerState1} ->
            Req3 = vegur_utils:set_handler_state(HandlerState1, Req2),
            handle_service(Service, Req3, Env);
        {error, CheckoutError, Req2, HandlerState1} ->
            Req3 = vegur_utils:set_handler_state(HandlerState1, Req2),
            handle_error(CheckoutError, Req3, Env)
    end.

-spec handle_service(Service, Req, Env) ->
                            {ok, Req, Env}|
                            {error, HttpCode, Req} when
      Service :: vegur_interface:service(),
      Req :: cowboy_req:req(),
      Env :: cowboy_middleware:env(),
      HttpCode :: cowboy:http_status().
handle_service(Service, Req, Env) ->
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    {ServiceBackend, ConnectOpts, Req2, HandlerState1} = service_backend(InterfaceModule, Service, Req1, HandlerState),
    Req3 = vegur_request_log:stamp(pre_connect, Req2),
    case ?LOG(connect_time, vegur_proxy:backend_connection(ServiceBackend, ConnectOpts), Req3) of
        {{connected, Client}, Req4} ->
            Req5 = cowboy_req:set_meta(backend_connection, Client, Req4),
            Req6 = vegur_utils:set_handler_state(HandlerState1, Req5),
            {ok, Req6, Env};
        {{error, Reason}, Req4} ->
            {DomainGroup, Req5} = cowboy_req:meta(domain_group, Req4),
            {ok, Req6, HandlerState2} = InterfaceModule:checkin_service(DomainGroup, Service, connecting, Reason, Req5, HandlerState1),
            Req7 = vegur_utils:set_handler_state(HandlerState2, Req6),
            lookup_service(Req7, Env)
    end.

-spec handle_error(Reason, Req, Env) ->
                          {error, HttpCode, Req} when
      Reason :: term(),
      Req :: cowboy_req:req(),
      Env :: cowboy_middleware:env(),
      HttpCode :: cowboy:http_status().
handle_error(Reason, Req, _Env) ->
    {HttpCode, Req1} = vegur_utils:handle_error(Reason, Req),
    {error, HttpCode, Req1}.

-spec service_backend(InterfaceModule, Service, Req, HandlerState) ->
                             {ServiceBackend, ConnectOpts, Upstream, HandlerState} when
      InterfaceModule :: module(),
      Service :: vegur_interface:service(),
      ServiceBackend :: vegur_interface:service_backend(),
      ConnectOpts :: [term()],
      Upstream :: vegur_interface:upstream(),
      HandlerState :: vegur_interface:handler_state(),
      Req :: cowboy_req:req().
service_backend(InterfaceModule, Service, Req, HandlerState) ->
    case InterfaceModule:service_backend(Service, Req, HandlerState) of
        {ServiceBackend, Req, HandlerState1} ->
            {ServiceBackend, [], Req, HandlerState1};
        {ServiceBackend, ConnectOpts, Req, HandlerState1} ->
            {ServiceBackend, ConnectOpts, Req, HandlerState1}
    end.
            
