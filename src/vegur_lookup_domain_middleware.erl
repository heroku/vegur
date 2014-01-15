-module(vegur_lookup_domain_middleware).

-behaviour(cowboy_middleware).
-include("vegur_log.hrl").
-export([execute/2]).

execute(Req, Env) ->
    % Check if this is a healthcheck request
    {Host, Req1} = cowboy_req:host(Req),
    {InterfaceModule, HandlerState, Req2} = vegur_utils:get_interface_module(Req1),
    Res = InterfaceModule:lookup_domain_name(Host, HandlerState),
    handle_domain_lookup(Res, Req2, Env).

-spec handle_domain_lookup({error, not_found, HandlerState} |
                           {redirect, Reason, DomainGroup, Domain, HandlerState} |
                           {ok, DomainGroup, HandlerState}, Req, Env) ->
                                  {error, ErrorCode, Req} |
                                  {halt, Req} |
                                  {ok, Req, Env} when
      Reason :: atom(),
      DomainGroup :: vegur_interface:domain_group(),
      Domain :: vegur_interface:domain(),
      HandlerState :: vegur_interface:handler_state(),
      ErrorCode :: cowboy:http_status().
handle_domain_lookup({error, not_found, HandlerState}, Req, _Env) ->
    % No app associated with the domain
    {InterfaceModule, _HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    {{HttpCode, ErrorHeaders, ErrorBody}, HandlerState1} = InterfaceModule:error_page(not_found, undefined, HandlerState),
    Req2 = vegur_utils:set_handler_state(HandlerState1, Req1),
    Req3 = vegur_utils:render_response(HttpCode, ErrorHeaders, ErrorBody, Req2),
    {halt, Req3};
handle_domain_lookup({redirect, _Reason, _DomainGroup, RedirectTo, HandlerState}, Req, _Env) ->
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
    Req6 = vegur_utils:set_handler_state(HandlerState, Req5),
    {halt, Req6};
handle_domain_lookup({ok, DomainGroup, HandlerState}, Req, Env) ->
    Req1 = cowboy_req:set_meta(domain_group, DomainGroup, Req),
    Req2 = vegur_utils:set_handler_state(HandlerState, Req1),
    {ok, Req2, Env}.

% Internal
get_proto(<<"https">>) ->
    <<"https">>;
get_proto(_) ->
    <<"http">>.
