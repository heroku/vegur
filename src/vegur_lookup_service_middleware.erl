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
-module(vegur_lookup_service_middleware).

-behaviour(cowboyku_middleware).
-include("vegur_log.hrl").
-export([execute/2]).

-spec execute(Req, Env) ->
                     {ok, Req, Env}|
                     {error, HttpCode, Req} when
      Req :: cowboyku_req:req(),
      Env :: cowboyku_middleware:env(),
      HttpCode :: cowboyku:http_status().
execute(Req, Env) ->
    lookup_service(Req, Env).

lookup_service(Req, Env) ->
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    {DomainGroup, Req1} = cowboyku_req:meta(domain_group, Req),
    case InterfaceModule:checkout_service(DomainGroup, Req1, HandlerState) of
        {service, Service, Req2, HandlerState1} ->
            Req3 = vegur_utils:set_handler_state(HandlerState1, Req2),
            handle_service(Service, Req3, Env);
        {error, CheckoutError, Req2, HandlerState1} ->
            Req3 = vegur_utils:set_handler_state(HandlerState1, Req2),
            handle_error(CheckoutError, Req3, Env)
    end.

-spec handle_service(Service, Req, Env) ->
                            {ok, Req, Env}|
                            {error, HttpCode, Req} when
      Service :: vegur_interface:service(),
      Req :: cowboyku_req:req(),
      Env :: cowboyku_middleware:env(),
      HttpCode :: cowboyku:http_status().
handle_service(Service, Req, Env) ->
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    {ServiceBackend, Req2, HandlerState1} = InterfaceModule:service_backend(Service, Req1, HandlerState),
    Req3 = vegur_request_log:stamp(pre_connect, Req2),
    case ?LOG(connect_time, vegur_proxy:backend_connection(ServiceBackend), Req3) of
        {{connected, Client}, Req4} ->
            Req5 = cowboyku_req:set_meta(backend_connection, Client, Req4),
            Req6 = vegur_utils:set_handler_state(HandlerState1, Req5),
            {ok, Req6, Env};
        {{error, Reason}, Req4} ->
            {DomainGroup, Req5} = cowboyku_req:meta(domain_group, Req4),
            {ok, Req6, HandlerState2} = InterfaceModule:checkin_service(DomainGroup, Service, connecting, Reason, Req5, HandlerState1),
            Req7 = vegur_utils:set_handler_state(HandlerState2, Req6),
            lookup_service(Req7, Env)
    end.

-spec handle_error(Reason, Req, Env) ->
                          {error, HttpCode, Req} when
      Reason :: term(),
      Req :: cowboyku_req:req(),
      Env :: cowboyku_middleware:env(),
      HttpCode :: cowboyku:http_status().
handle_error(Reason, Req, _Env) ->
    {HttpCode, Req1} = vegur_utils:handle_error(Reason, Req),
    {error, HttpCode, Req1}.
