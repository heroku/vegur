-module(vegur_interface).

-type domain() :: binary().
-type domain_group() :: term().
-type service() :: term().
-type queue_length() :: non_neg_integer().
-type wait_time() :: non_neg_integer().
-type service_backend() :: {inet:ip_address(), inet:port_number()}.
-type lookup_stats() :: [{atom(), term()}]|[].

-export_type([domain/0,
              domain_group/0,
              service/0,
              queue_length/0,
              wait_time/0,
              service_backend/0,
              lookup_stats/0]).

-callback lookup_domain_name(Domain) ->
    {error, not_found} |
    {redirect, Reason, domain_group(), Domain} |
    {ok, domain_group()} when
      Domain :: binary(),
      Reason :: atom().

-callback lookup_service(domain_group()) ->
    {route, service(), lookup_stats()} |
    {error, no_route_id} |
    {error, {backlog_timeout, queue_length(), wait_time()}} |
    {error, {backlog_too_deep, queue_length(), wait_time()}} |
    {error, {conn_limit_reached, queue_length(), wait_time()}} |
    {error, route_lookup_failed} |
    {error, no_web_processes} |
    {error, crashed} |
    {error, backends_quarantined} |
    {error, backends_starting} |
    {error, backends_idle} |
    {error, app_blank} |
    {error, app_not_found} |
    {error, app_lookup_failed}.

-callback app_mode(domain_group()) ->
    normal_mode|maintenance_mode.

-callback service_backend(service()) ->
    service_backend().
