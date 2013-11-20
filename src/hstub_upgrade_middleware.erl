-module(hstub_upgrade_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    % Check if this connection should be upgraded. We only support upgrades to websockets
    % at this time. If it's a websocket connection, validate it and mark it as such.
    Upgrade = cowboy_req:header(<<"upgrade">>, Req),    
    handle_upgrade(Upgrade, Req, Env).

% http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.42
handle_upgrade(undefined, Req, Env) ->
    % No Upgrade header
    {ok, Req, Env};
handle_upgrade(<<"websockets">>, Req, Env) ->
    % Websockets connection
    {SecWebSocketKey, Req1} = cowboy_req:header(<<"sec-websocket-key">>, Req),
    case SecWebSocketKey of
        undefined ->
            % The request is invalid
            {error, 400, Req};
        _Key ->
            % The request has a SecWebSocketKey. This is enough for us to deem it
            % a possible websocket connection. Mark it as such and pass it on.
            Req1 = cowboy_req:set_meta(websocket_connection, true, Req1),
            {ok, Req1, Env}
    end;
handle_upgrade(_, Req, _Env) ->
    {error, 400, Req}.
