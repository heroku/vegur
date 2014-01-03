-module(hstub_stub).

-behaviour(hstub_interface).

-export([lookup_domain_name/1,
         in_maintenance_mode/1,
         lookup_service/1,
         service_backend/1]).

-spec lookup_domain_name(Domain) -> 
                                {error, not_found} |
                                {redirect, Reason, DomainGroup, Domain} |
                                {ok, DomainGroup} when
      Domain :: hstub_interface:domain(),
      Reason :: atom(),
      DomainGroup :: hstub_interface:domain_group().
lookup_domain_name(_Domain) ->
    {ok, domain_group}.

-spec lookup_service(DomainGroup) ->
                            {route, Service} |
                            {error, no_route_id} |
                            {error, {backlog_timeout, QueueLength,
                                     WaitTime}} |
                            {error, {backlog_too_deep, QueueLength,
                                     WaitTime}} |
                            {error, {conn_limit_reached, QueueLength,
                                     WaitTime}} |
                            {error, route_lookup_failed} |
                            {error, no_web_processes} |
                            {error, crashed} |
                            {error, backends_quarantined} |
                            {error, backends_starting} |
                            {error, backends_idle} |
                            {error, app_blank} |
                            {error, app_not_found} |
                            {error, app_lookup_failed} when
      DomainGroup :: hstub_interface:domain_group(),
      Service :: hstub_interface:service(),
      QueueLength :: hstub_interface:queue_length(),
      WaitTime :: hstub_interface:wait_time().
lookup_service(_DomainGroup) ->
    {route, service}.

-spec in_maintenance_mode(DomainGroup) ->
                                 boolean() when
      DomainGroup :: term().
in_maintenance_mode(_DomainGroup) ->
    false.

-spec service_backend(hstub_interface:service()) ->
                             hstub_interface:service_backend().
service_backend(_Service) ->
    {{127,0,0,1}, 80}.

%% create_redirect(Domain) ->
%%     RootDomainToReplace = hstub_app:config(heroku_domain),
%%     RootDomainToReplaceWith = hstub_app:config(herokuapp_domain),
%%     re:replace(Domain, RootDomainToReplace, RootDomainToReplaceWith).
