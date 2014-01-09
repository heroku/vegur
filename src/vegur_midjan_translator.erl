-module(vegur_midjan_translator).

-define(LOGMODULE, vegur_request_log).

-export([run/2]).

run(MiddlewareModule, {Req, Env}) ->
    case MiddlewareModule:execute(Req, Env) of
        {ok, Req1, Env1} ->
            {next, {Req1, Env1}};
        {halt, _Req} = C ->
            %% The request is being halted, After that we run the logging
            %% middleware. The connection _will not_ be reused until
            %% after the logging middleware has run. It would be possible
            %% to flush the response here, but that would not help with
            %% reusing the socket.
            % ok = cowboy_req:ensure_response(Req, 204),
            {stop, C};
        {error, _StatusCode, _Req} = C ->
            % ok = cowboy_req:ensure_response(Req, StatusCode),
            {stop, C}
    end.    
