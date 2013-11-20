-module(hstub_lookup_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    % Check if this is a healthcheck request
    {Host, Req1} = cowboy_req:host(Req),
    Res = hstub_lookup:lookup_domain(Host),
    handle_domain_lookup(Res, Req1, Env).


handle_domain_lookup({error, not_found}, Req, Env) ->
    {error, 404, Req};
handle_domain_lookup({redirect, herokuapp_redirect, _DomainGroup, RedirectTo}, Req, _Env) ->
    %% REDIRECT
    {Path, Req2} = cowboy_req:path(Req),
    {Qs, Req3} = cowboy_req:qs(Req2),
    Qs2 = case Qs of
              <<>> -> <<>>;
              _ -> ["?", Qs]
          end,
    {HeaderValue, Req4} = cowboy_req:header(<<"x-forwarded-proto">>, Req3),
    Proto = get_proto(HeaderValue),
    FullLocation = [Proto, <<"://">>, RedirectTo, Path, Qs2],
    {ok, Req5} = cowboy_req:reply(301, [{"location", FullLocation}], Req4),
    {halt, Req5};
handle_domain_lookup({ok, SuperDomain, DomainGroup}, Req, Env) ->
    {error, 500, Req}.

% Internal
get_proto(<<"https">>) ->
    <<"https">>;
get_proto(_) ->
    <<"http">>.
