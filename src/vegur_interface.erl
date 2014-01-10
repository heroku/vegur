-module(vegur_interface).

-type domain() :: binary().
-type domain_group() :: term().
-type service() :: term().
-type queue_length() :: non_neg_integer().
-type wait_time() :: non_neg_integer().
-type service_backend() :: {inet:ip_address(), inet:port_number()}.
-type lookup_stats() :: [{atom(), term()}]|[].
-type service_state() :: normal|{error, term()}.

-export_type([domain/0,
              domain_group/0,
              service/0,
              queue_length/0,
              wait_time/0,
              service_backend/0,
              lookup_stats/0,
              service_state/0]).

-callback lookup_domain_name(Domain) ->
    {error, not_found} |
    {redirect, Reason, domain_group(), Domain} |
    {ok, domain_group()} when
      Domain :: binary(),
      Reason :: atom().

-callback checkout_service(domain_group(), LookupStats|undefined) ->
    {service, service(), LookupStats}|
    {error, CheckoutError, LookupStats} when
      CheckoutError :: atom(),
      LookupStats :: lookup_stats().

-callback checkin_service(Service, ServiceState) ->
    ok when
      Service :: service(),
      ServiceState :: service_state().

-callback error_page(ErrorReason, DomainGroup) ->
    {HttpCode, ErrorBody, ErrorHeaders} when
      ErrorReason :: term(),
      DomainGroup :: domain_group(),
      HttpCode :: pos_integer(),
      ErrorBody :: binary(),
      ErrorHeaders :: [{iolist(), iolist()}]|[].

-callback app_mode(domain_group()) ->
    normal_mode|maintenance_mode.

-callback service_backend(service()) ->
    service_backend().
