-module(hstub_lookup_service_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    {DomainGroup, Req1} = cowboy_req:meta(domain_group, Req),
    Service = hstub_lookup:lookup_service(DomainGroup),
    handle_service(Service, Req1, Env).

handle_service({route, Service}, Req, Env) ->
    % We have a service to route to, moving on
    Req1 = cowboy_req:set_meta(service, Service, Req),
    {ok, Req1, Env};
handle_service({error, no_route_id}, Req, _Env) ->
    % No route registered for this app, this should return the blank app
    {error, 502, Req};
handle_service({error, {backlog_timeout, _QueueLen, _WaitTime}}, Req, _Env) ->
    % The backend did not reply in time; H11
    {error, 503, Req};
handle_service({error, {backlog_too_deep, _QueueLen, _WaitTime}}, Req, _Env) ->
    % The backend has too many waiting requests; H11
    {error, 503, Req};
handle_service({error, {conn_limit_reached, _QueueLen, _WaitTime}}, Req, _Env) ->
    % The backend has reached is connection limit; H22
    {error, 503, Req};
handle_service({error, route_lookup_failed}, Req, _Env) ->
    % Could not lookup the route; H99
    {error, 503, Req};
handle_service({error, no_web_processes}, Req, _Env) ->
    % No web processes (app scaled to web=0); H14
    {error, 503, Req};
handle_service({error, crashed}, Req, _Env) ->
    % The backend crashed; H10
    {error, 503, Req};
handle_service({error, backends_quarantined}, Req, _Env) ->
    % All the backends are quarantied; H99
    {error, 503, Req};
handle_service({error, backends_starting}, Req, _Env) ->
    % The backends are starting and took too long; H20
    {error, 503, Req};
handle_service({error, backends_idle}, Req, _Env) ->
    % The backends idled; H99 (what does this mean?
    {error, 503, Req};
handle_service({error, app_blank}, Req, _Env) ->
    % The application had no routes. This error originates from
    % heroku_route. Maybe a race; H99
    {error, 503, Req};
handle_service({error, app_not_found}, Req, _Env) ->
    % The application could not be found. This error originates from
    % heroku_route. Maybe a race; 404
    {error, 404, Req};
handle_service({error, app_lookup_failed}, Req, _Env) ->
    % The application could not be looked up. This error originates from
    % deep inside heroku_route. Maybe a race; H99
    {error, 503, Req};
handle_service(_, Req, _Env) ->
    % Uncought error. This should be logged down and have a developer look
    % at it; H99
    {error, 503, Req}.
