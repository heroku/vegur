-module(vegur_interface).

-type domain() :: binary().
-type domain_group() :: term().
-type service() :: term().
-type queue_length() :: non_neg_integer().
-type wait_time() :: non_neg_integer().
-type service_backend() :: {inet:ip_address(), inet:port_number()}.
-type phase() :: lookup | checkout | connecting | connected.
-type service_state() :: normal|term().
-type handler_state() :: term().
-type terminate_reason() :: healthcheck|healthcheck_error|normal|error.
-type ms() :: non_neg_integer().
-type stat() :: {bytes_recv|bytes_sent, non_neg_integer()}|
                {route_time|connect_time|total_time, ms()}.
-type stats() :: [stat()]|[].
-type feature() :: deep_continue | peer_port.
-type upstream() :: cowboyku_req:req().

-export_type([domain/0,
              domain_group/0,
              service/0,
              queue_length/0,
              wait_time/0,
              service_backend/0,
              handler_state/0,
              phase/0,
              service_state/0,
              terminate_reason/0,
              stat/0,
              stats/0,
              ms/0,
              feature/0,
              upstream/0]).

-callback init(RequestAccepted, Upstream) ->
    {ok, Upstream, HandlerState} when
      RequestAccepted :: erlang:timestamp(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback lookup_domain_name(Domain, Upstream, HandlerState) ->
    {error, not_found, Upstream, HandlerState} |
    {redirect, Reason, DomainGroup, Domain, Upstream, HandlerState} |
    {ok, DomainGroup, Upstream, HandlerState} when
      Domain :: binary(),
      Reason :: atom(),
      DomainGroup :: domain_group(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback checkout_service(DomainGroup, Upstream, HandlerState) ->
    {service, service(), Upstream, HandlerState}|
    {error, CheckoutError, Upstream, HandlerState} when
      CheckoutError :: atom()|tuple(),
      DomainGroup :: domain_group(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback checkin_service(DomainGroup, Service, Phase, ServiceState, Upstream, HandlerState) ->
    {ok, Upstream, HandlerState} when
      DomainGroup :: domain_group(),
      Service :: service(),
      Phase :: phase(),
      ServiceState :: service_state(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback feature(feature(), HandlerState) ->
    {enabled | disabled, HandlerState} when
      HandlerState :: handler_state().

-callback error_page(ErrorReason, DomainGroup, Upstream, HandlerState) ->
    {{HttpCode, ErrorHeaders, ErrorBody}, Upstream, HandlerState} when
      ErrorReason :: term(),
      DomainGroup :: domain_group(),
      HttpCode :: pos_integer(),
      ErrorHeaders :: [{iodata(), iodata()}]|[],
      ErrorBody :: binary(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback service_backend(Service, Upstream, HandlerState) ->
    {ServiceBackend, Upstream, HandlerState} when
      Service :: service(),
      ServiceBackend :: service_backend(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback additional_headers(Direction, Logs, Upstream, HandlerState) ->
    {HeadersToAddOrReplace, HandlerState} when
      Direction :: upstream | downstream,
      Logs :: vegur_req_log:request_log(),
      Upstream :: upstream(),
      HeadersToAddOrReplace :: [{binary(), iodata()}],
      HandlerState :: handler_state().

-callback terminate(Reason, Upstream, HandlerState) ->
    any() when
      Reason :: terminate_reason(),
      Upstream :: upstream(),
      HandlerState :: handler_state().
