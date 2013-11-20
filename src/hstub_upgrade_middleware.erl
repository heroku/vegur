-module(hstub_upgrade_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    % Check if this connection should be upgraded. We only support upgrades to websockets
    % at this time. If it's a websocket connection, validate it and mark it as such.
    {ok, ConnectionTokens, Req1} = cowboy_req:parse_header(<<"connection">>, Req),
    case lists:member(<<"upgrade">>, ConnectionTokens) of
        false ->
            {ok, Req1, Env};
        true ->
            %% The connection should be upgraded
            case cowboy_req:parse_header(<<"upgrade">>, Req1) of
                {ok, Upgrade, Req2} ->
                    handle_upgrade(Upgrade, Req2, Env);
                {undefined, _, Req2} ->
                    {error, 400, Req2}; % 426?
                _ ->
                    {error, 400, Req1}
            end
    end.

% http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.42
-spec handle_upgrade(undefined|[binary()] | {error, term()}, Req, Env) ->
                            {ok, Req, Env} |
                            {error, 400, Req} when
      Req :: cowboy_req:req(),
      Env :: any().
handle_upgrade(undefined, Req, Env) ->
    % No Upgrade header
    {ok, Req, Env};
handle_upgrade([<<"websockets">>], Req, Env) ->
    % Websockets connection
    {SecWebSocketKey, Req1} = cowboy_req:header(<<"sec-websocket-key">>, Req),
    case SecWebSocketKey of
        undefined ->
            % The request is invalid
            {error, 400, Req};
        _Key ->
            % The request has a SecWebSocketKey. This is enough for us to deem it
            % a possible websocket connection. Mark it as such and pass it on.
            Req2 = cowboy_req:set_meta(websocket_connection, true, Req1),
            {ok, Req2, Env}
    end;
handle_upgrade({error, _}, Req, _Env) ->
    {error, 400, Req};
handle_upgrade(_, Req, _Env) ->
    % The upgrade header can contain other values, those will result in a client error
    {error, 400, Req}.

