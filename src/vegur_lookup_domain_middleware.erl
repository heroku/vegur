-module(vegur_lookup_domain_middleware).

-behaviour(cowboy_middleware).
-include("vegur_log.hrl").
-export([execute/2]).

execute(Req, Env) ->
    % Check if this is a healthcheck request
    InterfaceModule = vegur_utils:get_interface_module(Env),
    {Host, Req1} = cowboy_req:host(Req),
    {Res, Req2} = ?LOG(domain_lookup, InterfaceModule:lookup_domain_name(Host), Req1),
    handle_domain_lookup(Res, Req2, Env).

-spec handle_domain_lookup({error, not_found} |
                           {redirect, Reason, DomainGroup, Domain} |
                           {ok, DomainGroup}, Req, Env) ->
                                  {error, ErrorCode, Req} |
                                  {halt, Req} |
                                  {ok, Req, Env} when
      Reason :: atom(),
      DomainGroup :: vegur_interface:domain_group(),
      Domain :: vegur_interface:domain(),
      ErrorCode :: cowboy:http_status().
handle_domain_lookup({error, not_found}, Req, Env) ->
    % No app associated with the domain
    InterfaceModule = vegur_utils:get_interface_module(Env),
    {HttpCode, ErrorHeaders, ErrorBody} = InterfaceModule:error_page(not_found, undefined),
    {ok, Req1} = cowboy_req:reply(HttpCode, ErrorHeaders, ErrorBody, Req),
    {halt, Req1};
handle_domain_lookup({redirect, _Reason, _DomainGroup, RedirectTo}, Req, _Env) ->
    % This is a old app running on appname.heroku.com,
    % it should be redirected to appname.herokuapp.com.
    {Path, Req2} = cowboy_req:path(Req),
    {Qs, Req3} = cowboy_req:qs(Req2),
    Qs2 = case Qs of
              <<>> -> <<>>;
              _ -> ["?", Qs]
          end,
    {HeaderValue, Req4} = cowboy_req:header(<<"x-forwarded-proto">>, Req3),
    Proto = get_proto(HeaderValue),
    FullLocation = [Proto, <<"://">>, RedirectTo, Path, Qs2],
    {ok, Req5} = cowboy_req:reply(301, [{<<"location">>, FullLocation}], Req4),
    {halt, Req5};
handle_domain_lookup({ok, DomainGroup}, Req, Env) ->
    Req1 = cowboy_req:set_meta(domain_group, DomainGroup, Req),
    {ok, Req1, Env}.

% Internal
get_proto(<<"https">>) ->
    <<"https">>;
get_proto(_) ->
    <<"http">>.
