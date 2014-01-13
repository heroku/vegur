-module(vegur_stub).

-behaviour(vegur_interface).

-export([init/2,
         lookup_domain_name/2,
         checkout_service/2,
         checkin_service/4,
         service_backend/2,
         error_page/3]).

-spec init(RequestAccepted, RequestId) ->
                  {ok, HandlerState} when
      RequestAccepted :: erlang:timestamp(),
      RequestId :: binary(),
      HandlerState :: vegur_interface:handler_state().
init(_, _) ->
    {ok, undefined}.

-spec lookup_domain_name(Domain, HandlerState) -> 
                                {error, not_found, HandlerState} |
                                {redirect, Reason, DomainGroup, Domain, HandlerState} |
                                {ok, DomainGroup, HandlerState} when
      Domain :: vegur_interface:domain(),
      Reason :: atom(),
      DomainGroup :: vegur_interface:domain_group(),
      HandlerState :: vegur_interface:handler_state().
lookup_domain_name(_Domain, HandlerState) ->
    {ok, domain_group, HandlerState}.

-spec checkout_service(DomainGroup, HandlerState) ->
                              {service, Service, HandlerState} |
                              {error, CheckoutError, HandlerState} when
      CheckoutError :: atom(),
      DomainGroup :: vegur_interface:domain_group(),
      Service :: vegur_interface:service(),
      HandlerState :: vegur_interface:handler_state().
checkout_service(_DomainGroup, HandlerState) ->
    {service, service, HandlerState}.

-spec checkin_service(DomainGroup, Service, ServiceState, HandlerState) ->
                             {ok, HandlerState} when
      DomainGroup :: vegur_interface:domain_group(),
      Service :: vegur_interface:service(),
      ServiceState :: vegur_interface:service_state(),
      HandlerState :: vegur_interface:handler_state().
checkin_service(_DomainGroup, _Service, _ServiceState, HandlerState) ->
    {ok, HandlerState}.

-spec error_page(ErrorReason, DomainGroup, HandlerState) ->
                        {{HttpCode, ErrorHeaders, ErrorBody}, HandlerState} when
      ErrorReason :: term(),
      DomainGroup :: vegur_interface:domain_group(),
      HttpCode :: pos_integer(),
      ErrorHeaders :: [{iolist(), iolist()}]|[],
      ErrorBody :: binary(),
      HandlerState :: vegur_interface:handler_state().
error_page(not_found, _DomainGroup, HandlerState) ->
    {{404, [], <<>>}, HandlerState};
error_page(no_route_id, _DomainGroup, HandlerState) ->
    {{502, [], <<>>}, HandlerState};
error_page({backlog_timeout, 100, 100}, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page({backlog_too_deep, 100, 100}, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page({conn_limit_reached, 100, 100}, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page(route_lookup_failed, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page(no_web_processes, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page(crashed, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page(backends_quarantined, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page(backends_starting, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page(backends_idle, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page(app_blank, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page(app_not_found, _DomainGroup, HandlerState) ->
    {{404, [], <<>>}, HandlerState};
error_page(app_lookup_failed, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page(maintainance_mode, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState};
error_page(_, _DomainGroup, HandlerState) ->
    {{503, [], <<>>}, HandlerState}.

-spec service_backend(Service, HandlerState) ->
                             {ServiceBackend, HandlerState} when
      Service :: vegur_interface:service(),
      HandlerState :: vegur_interface:handler_state(),
      ServiceBackend :: vegur_interface:service_backend().
service_backend(_Service, HandlerState) ->
    {{{127,0,0,1}, 80}, HandlerState}.

%% create_redirect(Domain) ->
%%     RootDomainToReplace = vegur_app:config(heroku_domain),
%%     RootDomainToReplaceWith = vegur_app:config(herokuapp_domain),
%%     re:replace(Domain, RootDomainToReplace, RootDomainToReplaceWith).
