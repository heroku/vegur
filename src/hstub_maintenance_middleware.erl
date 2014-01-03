-module(hstub_maintenance_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    % Check if the app is in maintenance mode
    InterfaceModule = proplists:get_value(interface_module, Env),
    {DomainGroup, Req1} = cowboy_req:meta(domain_group, Req),
    MaintainanceMode = InterfaceModule:in_maintenance_mode(DomainGroup),
    handle_maintenance_mode(MaintainanceMode, Req1, Env).

-spec handle_maintenance_mode(boolean(), Req, Env) ->
                                     {ok, cowboy_req:req(), Env} |
                                     {error, ErrorCode, cowboy_req:req()} when
      Req :: cowboy_req:req(),
      Env :: cowboy_middleware:env(),
      ErrorCode :: 503.
handle_maintenance_mode(false, Req, Env) ->
    {ok, Req, Env};
handle_maintenance_mode(true, Req, _Env) ->
    % The app is in maintenance mode, serve information page
    {error, 503, Req}.
