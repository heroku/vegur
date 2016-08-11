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
-module(toy_router).
-behaviour(vegur_interface).
-export([init/2,
         terminate/3,
         lookup_domain_name/3,
         checkout_service/3,
         checkin_service/6,
         service_backend/3,
         feature/2,
         additional_headers/4,
         error_page/4]).

-record(state, {tries = [] :: list()}).

init(AcceptTime, Upstream) ->
    random:seed(AcceptTime), % RNGs require per-process seeding
    {ok, Upstream, #state{}}. % state initialization here.

lookup_domain_name(_AllDomains, Upstream, State) ->
    %% hardcoded values, we don't care about the domain
    Servers = [{1, {127,0,0,1}, 8081},
               {2, {127,0,0,1}, 8082}],
    {ok, Servers, Upstream, State}.

checkout_service(Servers, Upstream, State=#state{tries=Tried}) ->
    Available = Servers -- Tried,
    case Available of
        [] ->
            {error, all_blocked, Upstream, State};
        _ ->
            N = random:uniform(length(Available)),
            Pick = lists:nth(N, Available),
            {service, Pick, Upstream, State#state{tries=[Pick | Tried]}}
    end.

service_backend({_Id, IP, Port}, Upstream, State) ->
    %% extract the IP:PORT from the chosen server.
    %% To enable keep-alive, use:
    %% `{{keepalive, {default, {IP,Port}}}, Upstream, State}'
    %% To force the use of a new keepalive connection, use:
    %% `{{keepalive, {new, {IP,Port}}}, Upstream, State}'
    %% Otherwise, no keepalive is done to the back-end:
    {{IP, Port}, Upstream, State}.

checkin_service(_Servers, _Pick, _Phase, _ServState, Upstream, State) ->
    %% if we tracked total connections, we would decrement the counters here
    {ok, Upstream, State}.

feature(_WhoCares, State) ->
    {disabled, State}.

additional_headers(_Direction, _Log, _Upstream, State) ->
    {[], State}.

%% Vegur-returned errors that should be handled no matter what.
%% Full list in src/vegur_stub.erl
error_page({upstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
    %% Blame the caller
    {{400, [], <<>>}, Upstream, HandlerState};
error_page({downstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
    %% Blame the server
    {{500, [], <<>>}, Upstream, HandlerState};
error_page({undefined, _Reason}, _DomainGroup, Upstream, HandlerState) ->
    %% Who knows who was to blame!
    {{500, [], <<>>}, Upstream, HandlerState};
%% Specific error codes from middleware
error_page(empty_host, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(expectation_failed, _DomainGroup, Upstream, HandlerState) ->
    {{417, [], <<>>}, Upstream, HandlerState};
%% Catch-all
error_page(_, _DomainGroup, Upstream, HandlerState) ->
    {{500, [], <<>>}, Upstream, HandlerState}.

terminate(_, _, _) ->
    ok.
