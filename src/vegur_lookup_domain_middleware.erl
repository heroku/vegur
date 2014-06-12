-module(vegur_lookup_domain_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    {Host, Req1} = cowboy_req:host(Req),
    {InterfaceModule, HandlerState, Req2} = vegur_utils:get_interface_module(Req1),
    case InterfaceModule:lookup_domain_name(Host, Req2, HandlerState) of 
        {error, Reason, Req3, HandlerState1} ->
            Req4 = vegur_utils:set_handler_state(HandlerState1, Req3),
            handle_error(Reason, Req4, Env);
        {redirect, Reason, DomainGroup, Domain, Req3, HandlerState1} ->
            Req4 = vegur_utils:set_handler_state(HandlerState1, Req3),
            handle_redirect(Reason, DomainGroup, Domain, Req4, Env);
        {ok, DomainGroup, Req3, HandlerState1} ->
            Req4 = vegur_utils:set_handler_state(HandlerState1, Req3),
            Req5 = cowboy_req:set_meta(domain_group, DomainGroup, Req4),
            {ok, Req5, Env}
    end.

-spec handle_error(Reason, Req, Env) -> 
                          {error, HttpCode, Req} when
      Reason :: atom(),
      Req :: cowboy_req:req(),
      Env :: cowboy_middleware:env(),
      HttpCode :: cowboy:http_status().
handle_error(Reason, Req, _Env) ->
    {HttpCode, Req1} = vegur_utils:handle_error(Reason, Req),
    {error, HttpCode, Req1}.

-spec handle_redirect(Reason, DomainGroup, Domain, Req, Env) ->
                             {halt, HttpCode, Req} when
      Reason :: any(),
      DomainGroup :: vegur_interface:domain_group(),
      Domain :: vegur_interface:domain(),
      Req :: cowboy_req:req(),
      HttpCode :: cowboy:http_status(),
      Env :: cowboy_middleware:env().
handle_redirect(_Reason, _DomainGroup, RedirectTo, Req, _Env) ->
    {Path, Req1} = cowboy_req:path(Req),
    {Qs, Req2} = cowboy_req:qs(Req1),
    Qs2 = case Qs of
              <<>> -> <<>>;
              _ -> ["?", Qs]
          end,
    {HeaderValue, Req3} = cowboy_req:header(<<"x-forwarded-proto">>, Req2),
    Proto = get_proto(HeaderValue),
    FullLocation = [Proto, <<"://">>, RedirectTo, Path, Qs2],
    {ok, Req4} = cowboy_req:reply(301, [{<<"location">>, <<"location">>, FullLocation}], Req3),
    {halt, 301, Req4}.

% Internal
get_proto(<<"https">>) ->
    <<"https">>;
get_proto(_) ->
    <<"http">>.
