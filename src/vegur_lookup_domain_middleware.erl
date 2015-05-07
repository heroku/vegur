-module(vegur_lookup_domain_middleware).

-behaviour(cowboyku_middleware).
-export([execute/2]).

execute(Req, Env) ->
    {Host, Req1} = cowboyku_req:host(Req),
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
            Req5 = cowboyku_req:set_meta(domain_group, DomainGroup, Req4),
            {ok, Req5, Env}
    end.

-spec handle_error(Reason, Req, Env) -> 
                          {error, HttpCode, Req} when
      Reason :: atom(),
      Req :: cowboyku_req:req(),
      Env :: cowboyku_middleware:env(),
      HttpCode :: cowboyku:http_status().
handle_error(Reason, Req, _Env) ->
    {HttpCode, Req1} = vegur_utils:handle_error(Reason, Req),
    {error, HttpCode, Req1}.

-spec handle_redirect(Reason, DomainGroup, Domain, Req, Env) ->
                             {halt, HttpCode, Req} when
      Reason :: any(),
      DomainGroup :: vegur_interface:domain_group(),
      Domain :: vegur_interface:domain(),
      Req :: cowboyku_req:req(),
      HttpCode :: cowboyku:http_status(),
      Env :: cowboyku_middleware:env().
handle_redirect(_Reason, _DomainGroup, RedirectTo, Req, _Env) ->
    {FullLocation, Req2} = build_redirect_uri(RedirectTo, Req),
    {ok, Req3} = cowboyku_req:reply(301, [{<<"location">>, FullLocation}], Req2),
    {halt, 301, Req3}.

% Internal

build_redirect_uri(RedirectTo, Req) ->
    {Path, Req1} = cowboyku_req:path(Req),
    {Qs, Req2} = get_querystring(Req1),
    {HeaderValue, Req3} = cowboyku_req:header(<<"x-forwarded-proto">>, Req2),
    Proto = get_proto(HeaderValue),
    {[Proto, <<"://">>, RedirectTo, Path, Qs], Req3}.

get_querystring(Req) ->
    {Qs, Req2} = cowboyku_req:qs(Req),
    case Qs of
        <<>> -> {<<>>, Req2};
        _ -> {["?", Qs], Req2}
    end.

get_proto(<<"https">>) ->
    <<"https">>;
get_proto(_) ->
    <<"http">>.

