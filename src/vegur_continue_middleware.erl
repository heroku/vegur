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
%%%
%%% @doc The continue middleware is in charge of detecting whether a
%%% 100-continue expectation has been declared. If so, it will ask
%%% the callback module whether to instantly return the '100 Continue'
%%% response or whether it should be negotiated with the back-end.
%%%
%%% In the latter case, this module sets the `cowboyku_req:meta'
%%% `continue' entry to `continue' or leaves it undefined otherwise.
%%% @end
-module(vegur_continue_middleware).

-behaviour(cowboyku_middleware).
-export([execute/2]).

execute(Req, Env) ->
    case vegur_utils:parse_header(<<"expect">>, Req) of
        {ok, {[<<"100-continue">>], Req1}} ->
            %% We only have continue as an expectation
            {Req2, Env2} = handle_feature(Req1, Env),
            {ok, Req2, Env2};
        {ok, {[undefined], Req1}} ->
            %% no Expect header
            {ok, Req1, Env};
        {ok, {[], Req1}} ->
            %% Empty Expect header
            {ok, Req1, Env};
        {ok, _} ->
            %% Any other value for expect headers
            {HttpCode, Req1} = vegur_utils:handle_error(expectation_failed, Req),
            {error, HttpCode, Req1};
        {error, _} ->
            %% Bad request, invalid header value
            {HttpCode, Req1} = vegur_utils:handle_error(bad_request_header, Req),
            {error, HttpCode, Req1}
    end.

handle_feature(Req, Env) ->
    {InterfaceModule, HandlerState, Req2} = vegur_utils:get_interface_module(Req),
    case InterfaceModule:feature(deep_continue, HandlerState) of
        {enabled, HandlerState2} ->
            Req3 = vegur_utils:set_handler_state(HandlerState2, Req2),
            {cowboyku_req:set_meta(continue, continue, Req3), Env};
        {disabled, HandlerState2} ->
            Req3 = vegur_utils:set_handler_state(HandlerState2, Req2),
            Req4 = flush_expect_headers(Req3),
            send_continue(Req4),
            {Req4, Env}
    end.

flush_expect_headers(Req) ->
    Headers = cowboyku_req:get(headers, Req),
    NewHeaders = vegur_utils:delete_all_headers(<<"expect">>, Headers),
    cowboyku_req:set([{headers, NewHeaders}], Req).

send_continue(Req) ->
    {Transport, Socket} = vegur_utils:borrow_cowboyku_socket(Req),
    Transport:send(Socket, <<"HTTP/1.1 100 Continue\r\n\r\n">>).
