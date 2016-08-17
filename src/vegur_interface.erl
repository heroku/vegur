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
-module(vegur_interface).

-type domain() :: binary().
-type domain_group() :: term().
-type service() :: term().
-type queue_length() :: non_neg_integer().
-type wait_time() :: non_neg_integer().
-type service_backend() :: {inet:ip_address(), inet:port_number()}
                         | {keepalive, {default|new, {inet:ip_address(), inet:port_number()}}}.
-type phase() :: lookup | checkout | connecting | connected.
-type service_state() :: normal|term().
-type handler_state() :: term().
-type terminate_reason() :: healthcheck|healthcheck_error|normal|error.
-type ms() :: non_neg_integer().
-type stat() :: {bytes_recv|bytes_sent, non_neg_integer()}|
                {route_time|connect_time|total_time, ms()}.
-type stats() :: [stat()]|[].
-type feature() :: deep_continue | peer_port.
-type upstream() :: cowboyku_req:req().

-export_type([domain/0,
              domain_group/0,
              service/0,
              queue_length/0,
              wait_time/0,
              service_backend/0,
              handler_state/0,
              phase/0,
              service_state/0,
              terminate_reason/0,
              stat/0,
              stats/0,
              ms/0,
              feature/0,
              upstream/0]).

-callback init(RequestAccepted, Upstream) ->
    {ok, Upstream, HandlerState} when
      RequestAccepted :: erlang:timestamp(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback lookup_domain_name(Domain, Upstream, HandlerState) ->
    {error, atom(), Upstream, HandlerState} |
    {redirect, Reason, DomainGroup, Domain, Upstream, HandlerState} |
    {ok, DomainGroup, Upstream, HandlerState} when
      Domain :: binary(),
      Reason :: atom(),
      DomainGroup :: domain_group(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback checkout_service(DomainGroup, Upstream, HandlerState) ->
    {service, service(), Upstream, HandlerState}|
    {error, CheckoutError, Upstream, HandlerState} when
      CheckoutError :: atom()|tuple(),
      DomainGroup :: domain_group(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback checkin_service(DomainGroup, Service, Phase, ServiceState, Upstream, HandlerState) ->
    {ok, Upstream, HandlerState} when
      DomainGroup :: domain_group(),
      Service :: service(),
      Phase :: phase(),
      ServiceState :: service_state(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback feature(feature(), HandlerState) ->
    {enabled | disabled, HandlerState} when
      HandlerState :: handler_state().

-callback error_page(ErrorReason, DomainGroup, Upstream, HandlerState) ->
    {{HttpCode, ErrorHeaders, ErrorBody}, Upstream, HandlerState} when
      ErrorReason :: term(),
      DomainGroup :: domain_group(),
      HttpCode :: pos_integer(),
      ErrorHeaders :: [{iodata(), iodata()}]|[],
      ErrorBody :: binary(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback service_backend(Service, Upstream, HandlerState) ->
    {ServiceBackend, Upstream, HandlerState} when
      Service :: service(),
      ServiceBackend :: service_backend(),
      Upstream :: upstream(),
      HandlerState :: handler_state().

-callback additional_headers(Direction, Logs, Upstream, HandlerState) ->
    {HeadersToAddOrReplace, HandlerState} when
      Direction :: upstream | downstream,
      Logs :: vegur_req_log:request_log(),
      Upstream :: upstream(),
      HeadersToAddOrReplace :: [{binary(), iodata()}],
      HandlerState :: handler_state().

-callback terminate(Reason, Upstream, HandlerState) ->
    any() when
      Reason :: terminate_reason(),
      Upstream :: upstream(),
      HandlerState :: handler_state().
