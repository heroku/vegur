-module(vegur_stub).

-behaviour(vegur_interface).

-export([init/2,
         terminate/3,
         lookup_domain_name/3,
         checkout_service/3,
         checkin_service/5,
         service_backend/3,
         error_page/4]).

-spec init(RequestAccepted, Upstream) ->
                  {ok, Upstream, HandlerState} when
      RequestAccepted :: erlang:timestamp(),
      Upstream :: vegur_interface:upstream(),
      HandlerState :: vegur_interface:handler_state().
init(_, Upstream) ->
    {ok, Upstream, undefined}.

-spec lookup_domain_name(Domain, Upstream, HandlerState) -> 
                                {error, not_found, Upstream, HandlerState} |
                                {redirect, Reason, DomainGroup, Domain, Upstream, HandlerState} |
                                {ok, DomainGroup, Opts, Upstream, HandlerState} when
      Domain :: vegur_interface:domain(),
      Reason :: atom(),
      DomainGroup :: vegur_interface:domain_group(),
      HandlerState :: vegur_interface:handler_state(),
      Opts :: vegur_features:options(),
      Upstream :: vegur_interface:upstream().
lookup_domain_name(_Domain, Upstream, HandlerState) ->
    {ok, domain_group, [deep_continue], Upstream, HandlerState}.

-spec checkout_service(DomainGroup, Upstream, HandlerState) ->
                              {service, Service, Upstream, HandlerState} |
                              {error, CheckoutError, Upstream, HandlerState} when
      CheckoutError :: atom(),
      DomainGroup :: vegur_interface:domain_group(),
      Service :: vegur_interface:service(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
checkout_service(_DomainGroup, Upstream, HandlerState) ->
    {service, service, Upstream, HandlerState}.

-spec checkin_service(DomainGroup, Service, ServiceState, Upstream, HandlerState) ->
                             {ok, Upstream, HandlerState} when
      DomainGroup :: vegur_interface:domain_group(),
      Service :: vegur_interface:service(),
      ServiceState :: vegur_interface:service_state(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
checkin_service(_DomainGroup, _Service, _ServiceState, Upstream, HandlerState) ->
    {ok, Upstream, HandlerState}.

-spec error_page(ErrorReason, DomainGroup, Upstream, HandlerState) ->
                        {{HttpCode, ErrorHeaders, ErrorBody}, Upstream, HandlerState} when
      ErrorReason :: term(),
      DomainGroup :: vegur_interface:domain_group(),
      HttpCode :: pos_integer(),
      ErrorHeaders :: [{iolist(), iolist()}]|[],
      ErrorBody :: binary(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
error_page(not_found, _DomainGroup, Upstream, HandlerState) ->
    {{404, [], <<>>}, Upstream, HandlerState};
error_page(no_route_id, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({backlog_timeout, 100, 100}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({backlog_too_deep, 100, 100}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({conn_limit_reached, 100, 100}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(route_lookup_failed, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(no_web_processes, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(crashed, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(backends_quarantined, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(backends_starting, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(backends_idle, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(app_blank, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(app_not_found, _DomainGroup, Upstream, HandlerState) ->
    {{404, [], <<>>}, Upstream, HandlerState};
error_page(app_lookup_failed, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(maintainance_mode, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, content_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, cookie_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, header_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, status_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page(empty_host, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(expectation_failed, _DomainGroup, Upstream, HandlerState) ->
    {{417, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(_, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState}.

-spec service_backend(Service, Upstream, HandlerState) ->
                             {ServiceBackend, Upstream, HandlerState} when
      Service :: vegur_interface:service(),
      HandlerState :: vegur_interface:handler_state(),
      ServiceBackend :: vegur_interface:service_backend(),
      Upstream :: vegur_interface:upstream().
service_backend(_Service, Upstream, HandlerState) ->
    {{{127,0,0,1}, 80}, Upstream, HandlerState}.

-spec terminate(Status, Upstream, HandlerState) -> ok when
      Status :: vegur_interface:terminate_reason(),
      Upstream :: vegur_interface:upstream(),
      HandlerState :: vegur_interface:handler_state().
terminate(_, _, _) ->
    ok.
