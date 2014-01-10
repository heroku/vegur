-module(vegur_stub).

-behaviour(vegur_interface).

-export([lookup_domain_name/1,
         app_mode/1,
         checkout_service/2,
         checkin_service/2,
         service_backend/1,
         error_page/2]).

-spec lookup_domain_name(Domain) -> 
                                {error, not_found} |
                                {redirect, Reason, DomainGroup, Domain} |
                                {ok, DomainGroup} when
      Domain :: vegur_interface:domain(),
      Reason :: atom(),
      DomainGroup :: vegur_interface:domain_group().
lookup_domain_name(_Domain) ->
    {ok, domain_group}.

-spec checkout_service(DomainGroup, LookupStats) ->
                              {route, Service, LookupStats} |
                              {error, CheckoutError, HttpCode, ErrorPage,
                               ErrorHeaders, LookupStats} when
      CheckoutError :: atom(),
      HttpCode :: pos_integer(),
      ErrorPage :: iolist()|binary(),
      ErrorHeaders :: [{iolist(), iolist()}]|[],
      DomainGroup :: vegur_interface:domain_group(),
      Service :: vegur_interface:service(),
      LookupStats :: vegur_interface:lookup_stats().
checkout_service(_DomainGroup, LookupStats) ->
    {route, service, LookupStats}.

-spec checkin_service(Service, ServiceState) ->
                             ok when
      Service :: vegur_interface:service(),
      ServiceState :: vegur_interface:service_state().
checkin_service(_Service, _ServiceState) ->
    ok.

-spec error_page(ErrorReason, DomainGroup) ->
                            {HttpCode, ErrorBody, ErrorHeaders} when
      ErrorReason :: term(),
      DomainGroup :: vegur_interface:domain_group(),
      HttpCode :: pos_integer(),
      ErrorBody :: binary(),
      ErrorHeaders :: [{iolist(), iolist()}]|[].
error_page(not_found, _DomainGroup) ->
    {404, <<>>, []};
error_page(no_route_id, _DomainGroup) ->
    {502, <<>>, []};
error_page({backlog_timeout, 100, 100}, _DomainGroup) ->
    {503, <<>>, []};
error_page({backlog_too_deep, 100, 100}, _DomainGroup) ->
    {503, <<>>, []};
error_page({conn_limit_reached, 100, 100}, _DomainGroup) ->
    {503, <<>>, []};
error_page(route_lookup_failed, _DomainGroup) ->
    {503, <<>>, []};
error_page(no_web_processes, _DomainGroup) ->
    {503, <<>>, []};
error_page(crashed, _DomainGroup) ->
    {503, <<>>, []};
error_page(backends_quarantined, _DomainGroup) ->
    {503, <<>>, []};
error_page(backends_starting, _DomainGroup) ->
    {503, <<>>, []};
error_page(backends_idle, _DomainGroup) ->
    {503, <<>>, []};
error_page(app_blank, _DomainGroup) ->
    {503, <<>>, []};
error_page(app_not_found, _DomainGroup) ->
    {404, <<>>, []};
error_page(app_lookup_failed, _DomainGroup) ->
    {503, <<>>, []};
error_page(_, _DomainGroup) ->
    {503, <<>>, []}.

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
