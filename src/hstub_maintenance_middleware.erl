-module(hstub_maintenance_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    % Check if the app is in maintenance mode
    {DomainGroup, Req1} = cowboy_req:meta(domain_group, Req),
    MaintainanceMode = hstub_domains:in_maintenance_mode(DomainGroup),
    handle_maintenance_mode(MaintainanceMode, Req1, Env).

-spec handle_maintenance_mode(boolean(), Req, Env) ->
                                     {ok, cowboy_req:req(), Env} |
                                     {error, 503, cowboy_req:req()} when
      Req :: cowboy_req:req(),
      Env :: any().
handle_maintenance_mode(false, Req, Env) ->
    {ok, Req, Env};
handle_maintenance_mode(true, Req, _Env) ->
    % The app is in maintenance mode, serve information page
    {error, 503, Req}.
