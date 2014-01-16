-module(vegur_interface).

-type domain() :: binary().
-type domain_group() :: term().
-type service() :: term().
-type queue_length() :: non_neg_integer().
-type wait_time() :: non_neg_integer().
-type service_backend() :: {inet:ip_address(), inet:port_number()}.
-type service_state() :: normal|term().
-type handler_state() :: term().
-type terminate_reason() :: healthcheck|healthcheck_error|normal|error.
-type ms() :: non_neg_integer().
-type stat() :: {bytes_recv|bytes_sent, non_neg_integer()}|
                {route_time|connect_time|total_time, ms()}.
-type stats() :: [stat()]|[].

-export_type([domain/0,
              domain_group/0,
              service/0,
              queue_length/0,
              wait_time/0,
              service_backend/0,
              handler_state/0,
              service_state/0,
              terminate_reason/0,
              stat/0,
              stats/0,
              ms/0]).

-callback init(RequestAccepted, RequestId) ->
    {ok, HandlerState} when
      RequestAccepted :: erlang:timestamp(),
      RequestId :: binary(),
      HandlerState :: handler_state().

-callback lookup_domain_name(Domain, HandlerState) ->
    {error, not_found, HandlerState} |
    {redirect, Reason, domain_group(), Domain, HandlerState} |
    {ok, domain_group(), HandlerState} when
      Domain :: binary(),
      Reason :: atom(),
      HandlerState :: handler_state().

-callback checkout_service(domain_group(), HandlerState) ->
    {service, service(), HandlerState}|
    {error, CheckoutError, HandlerState} when
      CheckoutError :: atom(),
      HandlerState :: handler_state().

-callback checkin_service(DomainGroup, Service, ServiceState, HandlerState) ->
    {ok, HandlerState} when
      DomainGroup :: domain_group(),
      Service :: service(),
      ServiceState :: service_state(),
      HandlerState :: handler_state().

-callback error_page(ErrorReason, DomainGroup, HandlerState) ->
    {{HttpCode, ErrorHeaders, ErrorBody}, HandlerState} when
      ErrorReason :: term(),
      DomainGroup :: domain_group(),
      HttpCode :: pos_integer(),
      ErrorHeaders :: [{iolist(), iolist()}]|[],
      ErrorBody :: binary(),
      HandlerState :: handler_state().

-callback service_backend(Service, HandlerState) ->
    {ServiceBackend, HandlerState} when
      Service :: service(),
      HandlerState :: handler_state(),
      ServiceBackend :: service_backend().

-callback terminate(Reason, Stats, HandlerState) ->
    any() when
      Reason :: terminate_reason(),
      Stats :: stats(),
      HandlerState :: handler_state().
