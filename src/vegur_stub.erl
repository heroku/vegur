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
%%% @doc Reference implementation of a trivial router callback module for vegur.
%%%
%%% This one does very little except route requests as they are to a pre-
%%% configured endpoint and be useful for vegur's own testing purposes.
%%% It is expected to be replaced by a custom one when being used in an
%%% actual system.
%%% @end
-module(vegur_stub).

-behaviour(vegur_interface).

-export([init/2,
         terminate/3,
         lookup_domain_name/3,
         checkout_service/3,
         checkin_service/6,
         service_backend/3,
         feature/2,
         additional_headers/4,
         error_page/4,
         instance_name/0]).

-record(state, {
           connect_tries = 0 :: non_neg_integer()
          ,features = [] :: [atom()]
         }).


 %% @doc The init function get called as soon as the request has been accepted
 %% and the basic headers parsed. This is the earliest point at which control
 %% is yielded to the router, and lets you set up initial data such as storing
 %% metrics, seeding RNGs, and whatever else the user may need.
-spec init(RequestAccepted, Upstream) ->
                  {ok, Upstream, HandlerState} when
      RequestAccepted :: erlang:timestamp(),
      Upstream :: vegur_interface:upstream(),
      HandlerState :: vegur_interface:handler_state().
init(_, Upstream) ->
    {ok, Upstream, #state{}}.

%% @doc When the proxy figures out the domain name a request was intended for,
%% Vegur will ask your router module to figure out if it belongs to you, and if
%% so, how.
%%
%% The expected return value in the successful case is a 'domain group', which
%% is an abstraction standing for a given application or set of back-end
%% instances. This has been chosen because a same application may have
%% multiple domains (say example.org, www.example.org, and example.com).
%%
%% The domain group can be any term. If you do not want to use this
%% abstraction, returning a server IP will also work, but the following
%% callbacks will still be called.
%%
%% A redirection can also be returned as a possible value, returning a 301
%% to the user.
%%
%% Returning `{error, not_found, Upstream, State}' will respond with an
%% undefined error code -- to be handled and configured in
%% {@link error_page/4}.
-spec lookup_domain_name(Domain, Upstream, HandlerState) ->
                                {error, not_found, Upstream, HandlerState} |
                                {redirect, Reason, DomainGroup, Domain, Upstream, HandlerState} |
                                {ok, DomainGroup, Upstream, HandlerState} when
      Domain :: vegur_interface:domain(),
      Reason :: atom(),
      DomainGroup :: vegur_interface:domain_group(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
lookup_domain_name(_Domain, Upstream, HandlerState) ->
    {ok, domain_group, Upstream, HandlerState}.

%% @doc A service is a subsection of a 'domain group', representing a set or
%% a subset of available endpoints. Checking it out may or may not be done
%% from different storage sets than domain groups -- or require different
%% levels of error handling, for example.
%%
%% This also lets you put restrictions across specific sections of an
%% application or endpoint such as rate-limiting and whatnot.
-spec checkout_service(DomainGroup, Upstream, HandlerState) ->
                              {service, Service, Upstream, HandlerState} |
                              {error, CheckoutError, Upstream, HandlerState} when
      CheckoutError :: atom()|tuple(),
      DomainGroup :: vegur_interface:domain_group(),
      Service :: vegur_interface:service(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
checkout_service(_App, Connection, #state{connect_tries=10}=HandlerState) ->
    {error, econn_refused, Connection, HandlerState};
checkout_service(maintenance_mode, Connection, HandlerState) ->
    {error, maintenance_mode, Connection, HandlerState};
checkout_service(_DomainGroup, Upstream, HandlerState) ->
    {service, service, Upstream, HandlerState}.

%% @doc Once the request is over, the resource is checked back in to allow for
%% usage tracking, rate-limiting, and so on. The `Phase' variable lets you
%% know whether a failure happened while `connecting' or once `connected'.
%% Similarly, the `ServiceState' variable lets you know if the operations ended
%% normally (`normal') or not (other).
-spec checkin_service(DomainGroup, Service, Phase, ServiceState, Upstream, HandlerState) ->
                             {ok, Upstream, HandlerState} when
      DomainGroup :: vegur_interface:domain_group(),
      Service :: vegur_interface:service(),
      Phase :: vegur_interface:phase(),
      ServiceState :: vegur_interface:service_state(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
checkin_service(_DomainGroup, _Service, _Phase, _ServiceState, Upstream, HandlerState=#state{connect_tries=Tries}) ->
    {ok, Upstream, HandlerState#state{connect_tries=Tries+1}}.

%% @doc When a service has been chosen, this function is called to extract the
%% IP and port of the service itself. This allows the callback module to carry
%% around fancier data structures to represent services while still leaving
%% vegur able to extract core information.
-spec service_backend(Service, Upstream, HandlerState) ->
                             {ServiceBackend, Upstream, HandlerState} when
      Service :: vegur_interface:service(),
      HandlerState :: vegur_interface:handler_state(),
      ServiceBackend :: vegur_interface:service_backend(),
      Upstream :: vegur_interface:upstream().
service_backend(_Service, Upstream, HandlerState) ->
    {{{127,0,0,1}, 80}, Upstream, HandlerState}.

%% @doc Specific features or behaviour are configurable conditionally, on
%% demand, on a per-request basis.
%% The two currently supported features are: 1) `deep_continue', which allows
%% to choose to pass and respect the `expect: 100-continue' headers, or to have
%% the proxy answer on behalf of the backends, and 2) `peer_port' which allows
%% to inject headers about the original peer IP address when working in TCP
%% Proxy mode.
-spec feature(Feature, HandlerState) -> {enabled | disabled, HandlerState} when
      Feature :: vegur_interface:feature(),
      HandlerState :: vegur_interface:handler_state().
feature(deep_continue, State) ->
    {enabled, State};
feature(peer_port, State) ->
    {disabled, State};
feature(_, State) ->
    {disabled, State}.

%% @doc Lets you inject custom HTTP headers in requests to a back-end, in order
%% to provide additional information.
-spec additional_headers(Direction, Log, Upstream, HandlerState) ->
    {HeadersToAddOrReplace, HandlerState} when
      Direction :: upstream | downstream,
      Log :: vegur_req_log:request_log(),
      Upstream :: vegur_interface:upstream(),
      HeadersToAddOrReplace :: [{binary(), iodata()}],
      HandlerState :: vegur_interface:handler_state().
additional_headers(upstream, Log, _Upstream, HandlerState=#state{features=Features}) ->
    case lists:member(router_metrics, Features) of
        true ->
            ConnectDuration = vegur_req_log:connect_duration(Log),
            StartToProxy = vegur_req_log:start_to_proxy_duration(Log),
            Headers = [{<<"Instance-Name">>, instance_name()}
                      ,{<<"Connect-Time">>, list_to_binary(integer_to_list(ConnectDuration))}
                      ,{<<"Total-Route-Time">>, list_to_binary(integer_to_list(StartToProxy))}],
            {Headers, HandlerState};
        _ ->
            {[], HandlerState}
    end;
additional_headers(downstream, _Log, _Upstream, HandlerState) ->
    %% We have nothing special to send to the user
    {[], HandlerState}.

%% @doc Takes any error reason returned in the other callbacks and lets you
%% define custom error pages when a back-end application couldn't do it or be
%% found.
%% The list in this function example is exhaustive of everything that could
%% be returned by vegur itself.
-spec error_page(ErrorReason, DomainGroup, Upstream, HandlerState) ->
                        {{HttpCode, ErrorHeaders, ErrorBody}, Upstream, HandlerState} when
      ErrorReason :: term(),
      DomainGroup :: vegur_interface:domain_group(),
      HttpCode :: pos_integer(),
      ErrorHeaders :: [{iodata(), iodata()}]|[],
      ErrorBody :: binary(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
%% Example errors that could be returned by this module and end up in here,
%% and can also be used by the existing test suite.
error_page(not_found, _DomainGroup, Upstream, HandlerState) ->
    {{404, [], <<>>}, Upstream, HandlerState};
error_page(no_route_id, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({backlog_timeout, _QueueLen, _WaitTime}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({backlog_too_deep, _QueueLen, _WaitTime}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({conn_limit_reached, _QueueLen, _WaitTime}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(route_lookup_failed, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(no_web_processes, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(crashed, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(backends_quarantined, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(backends_starting, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(backends_idle, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(app_not_found, _DomainGroup, Upstream, HandlerState) ->
    {{404, [], <<>>}, Upstream, HandlerState};
error_page(app_lookup_failed, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(maintainance_mode, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(econn_timeout, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(econn_refused, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(app_blank, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({app_blank, _App}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
%% Vegur-returned errors that should be handled no matter what
error_page(expectation_failed, _DomainGroup, Upstream, HandlerState) ->
    {{417, [], <<>>}, Upstream, HandlerState};
error_page({upstream, closed}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({upstream, etimedout}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({upstream, enotconn}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, closed}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, etimedout}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, enotconn}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, econnrefused}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, timeout}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({upstream, timeout}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({undefined, timeout}, _DomainGroup, Upstream, HandlerState) ->
    %% Who knows who timed out. Technically both!
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, invalid_status}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, content_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, cookie_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, header_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, status_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({upstream, {bad_chunk,_}}, _DomainGroup, Upstream, HandlerState) ->
    %% bad chunked encoding from client
    {{400, [], <<>>}, Upstream, HandlerState};
error_page({upstream, invalid_transfer_encoding}, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page({downstream, {bad_chunk,_}}, _DomainGroup, Upstream, HandlerState) ->
    %% bad chunked encoding from server
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, non_terminal_status_after_continue}, _DomainGroup, Upstream, HandlerState) ->
    %% Can't send a 1xx status after a 100 continue (except for upgrades)
    %% when expect: 100-continue is declared
    {{502, [], <<>>}, Upstream, HandlerState};
%% Generic handling
error_page(empty_host, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request_header, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(_, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState}.

instance_name() ->
    <<"vegur_stub">>.

-spec terminate(Status, Upstream, HandlerState) -> ok when
      Status :: vegur_interface:terminate_reason(),
      Upstream :: vegur_interface:upstream(),
      HandlerState :: vegur_interface:handler_state().
terminate(_, _, _) ->
    ok.
