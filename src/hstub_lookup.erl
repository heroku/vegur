-module(hstub_lookup).

-export([lookup_domain/1,
         lookup_service/1]).

-type domain() :: binary().
-type redirect_reason() :: herokuapp_redirect.

-export_type([domain/0,
              redirect_reason/0
             ]).

-spec lookup_domain(domain()) ->
                           {error, not_found} |
                           {redirect, redirect_reason(), hstub_domains:domain_group(), domain()} |
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
                            hstub_service:service_lookup_result().
lookup_service(DomainGroup) ->
    hstub_service:lookup(DomainGroup).

%% Internal
create_redirect(Domain) ->
    RootDomainToReplace = hstub_app:config(heroku_domain),
    RootDomainToReplaceWith = hstub_app:config(herokuapp_domain),
    re:replace(Domain, RootDomainToReplace, RootDomainToReplaceWith).
