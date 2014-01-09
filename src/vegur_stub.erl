-module(vegur_stub).

-behaviour(vegur_interface).

-export([lookup_domain_name/1,
         app_mode/1,
         lookup_service/1,
         service_backend/1]).

-spec lookup_domain_name(Domain) -> 
                                {error, not_found} |
                                {redirect, Reason, DomainGroup, Domain} |
                                {ok, DomainGroup} when
      Domain :: vegur_interface:domain(),
      Reason :: atom(),
      DomainGroup :: vegur_interface:domain_group().
lookup_domain_name(_Domain) ->
    {ok, domain_group}.

-spec lookup_service(DomainGroup) ->
                            {route, Service, LookupStats} |
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
      DomainGroup :: vegur_interface:domain_group(),
      Service :: vegur_interface:service(),
      QueueLength :: vegur_interface:queue_length(),
      WaitTime :: vegur_interface:wait_time(),
      LookupStats :: vegur_interface:lookup_stats().
lookup_service(_DomainGroup) ->
    {route, service, []}.

-spec app_mode(DomainGroup) ->
                      normal_mode when
      DomainGroup :: term().
app_mode(_DomainGroup) ->
    normal_mode.

-spec service_backend(vegur_interface:service()) ->
                             vegur_interface:service_backend().
service_backend(_Service) ->
    {{127,0,0,1}, 80}.

%% create_redirect(Domain) ->
%%     RootDomainToReplace = vegur_app:config(heroku_domain),
%%     RootDomainToReplaceWith = vegur_app:config(herokuapp_domain),
%%     re:replace(Domain, RootDomainToReplace, RootDomainToReplaceWith).
