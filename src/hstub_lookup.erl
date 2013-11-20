-module(hstub_lookup).

-export([lookup_domain/1,
         lookup_service/1]).

-type domain() :: binary().
-type reason() :: herokuapp_redirect.

-export_type([domain/0,
              reason/0
             ]).

-spec lookup_domain(domain()) ->
                           {error, not_found} |
                           {redirect, reason(), hstub_domains:domain_group(), domain()} |
                           {ok, hstub_domains:domain_group()}.
lookup_domain(Domain) ->
    case hstub_domains:lookup(Domain) of
        undefined ->
            {error, not_found};
        {error, {herokuapp, DomainGroup, SuperDomain}} ->
            RedirectTo = create_redirect(SuperDomain),
            {redirect, herokuapp_redirect, DomainGroup, RedirectTo};
        {ok, _SuperDomain, DomainGroup} ->
            {ok, DomainGroup}
    end.

-spec lookup_service(hstub_domains:domain_group()) ->
                            {route, hstub_services:service()} |
                            {error, no_route_id} |
                            {error, {backlog_timeout, QueueLen, WaitTime}} |
                            {error, {backlog_too_deep, QueueLen, WaitTime}} |
                            {error, {conn_limit_reached, QueueLen, WaitTime}} |
                            {error, route_lookup_failed} |
                            {error, no_web_processes} |
                            {error, crashed} |
                            {error, backends_quarantined} |
                            {error, backends_starting} |
                            {error, backends_idle} |
                            {error, app_blank} |
                            {error, app_not_found} |
                            {error, app_lookup_failed}.
lookup_service(DomainGroup) ->
    hstub_service:lookup(DomainGroup).

%% Internal
create_redirect(Domain) ->
    RootDomainToReplace = hstub_app:config(heroku_domain),
    RootDomainToReplaceWith = hstub_app:config(herokuapp_domain),
    re:replace(Domain, RootDomainToReplace, RootDomainToReplaceWith).
