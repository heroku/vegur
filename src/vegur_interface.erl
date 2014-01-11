-module(vegur_interface).

-type domain() :: binary().
-type domain_group() :: term().
-type service() :: term().
-type queue_length() :: non_neg_integer().
-type wait_time() :: non_neg_integer().
-type service_backend() :: {inet:ip_address(), inet:port_number()}.
-type lookup_stats() :: [{atom(), term()}]|[].
-type service_state() :: normal|term().

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

-callback checkin_service(DomainGroup, Service, ServiceState, LookupStats|undefined) ->
    {ok, LookupStats|undefined} when
      DomainGroup :: domain_group(),
      Service :: service(),
      ServiceState :: service_state(),
      LookupStats :: lookup_stats().

-callback error_page(ErrorReason, DomainGroup) ->
    {HttpCode, ErrorHeaders, ErrorBody} when
      ErrorReason :: term(),
      DomainGroup :: domain_group(),
      HttpCode :: pos_integer(),
      ErrorHeaders :: [{iolist(), iolist()}]|[],
      ErrorBody :: binary().

-callback service_backend(service()) ->
    service_backend().
