%%% Copyright (c) 2013-2015, Heroku Inc <routing-feedback@heroku.com>.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(vegur_upgrade_middleware).

-behaviour(cowboyku_middleware).
-export([execute/2]).

execute(Req, Env) ->
    match_headers(vegur_utils:parse_header(<<"connection">>, Req), Req, Env).

match_headers({ok,{ConnectionTokens, Req1}}, _, Env) ->
    case lists:member(<<"upgrade">>, ConnectionTokens) of
        false ->
            {ok, Req1, Env};
        true ->
            %% The connection should be upgraded
            case cowboyku_req:parse_header(<<"upgrade">>, Req1) of
                {ok, undefined, Req2} ->
                    {HttpCode, Req3} = vegur_utils:handle_error(bad_request_header, Req2),
                    {error, HttpCode, Req3};
                {ok, Upgrade, Req2} ->
                    handle_upgrade(Upgrade, Req2, Env);
                {undefined, _, Req2} ->
                    {HttpCode, Req3} = vegur_utils:handle_error(bad_request_header, Req2),
                    {error, HttpCode, Req3}; % 426?
                _ ->
                    {HttpCode, Req2} = vegur_utils:handle_error(bad_request_header, Req1),
                    {error, HttpCode, Req2}
            end
    end;
match_headers({error,_}, Req, _Env) ->
    {HttpCode, Req1} = vegur_utils:handle_error(bad_request_header, Req),
    {error, HttpCode, Req1}.

% http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.42
-spec handle_upgrade(undefined|[binary()] | {error, term()}, Req, Env) ->
                            {ok, Req, Env} |
                            {error, ErrorCode, Req} when
      Req :: cowboyku_req:req(),
      Env :: cowboyku_middleware:env(),
      ErrorCode :: 400.
handle_upgrade(undefined, Req, Env) ->
    % No Upgrade header
    {ok, Req, Env};
handle_upgrade(UpgradeTokens, Req, Env) when is_list(UpgradeTokens) ->
    {Type, Req1} = cowboyku_req:meta(request_type, Req, []),
    Req2 = cowboyku_req:set_meta(request_type, [upgrade|Type], Req1),
    {ok, Req2, Env};
handle_upgrade({error, _}, Req, _Env) ->
    Req1 = vegur_utils:set_request_status(error, Req),
    % @todo add custom errors
    {error, 400, Req1};
handle_upgrade(_, Req, _Env) ->
    % The upgrade header can contain other values, those will result in a client error
    Req1 = vegur_utils:set_request_status(error, Req),
    % @todo add custom errors
    {error, 400, Req1}.
