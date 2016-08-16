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
-module(vegur_midjan_middleware).
-behaviour(cowboyku_middleware).

-export([execute/2]).

%% Starting in OTP 19 dialyzer is kind of mad at this for no good reason
%% only complaining about 'no_return'.
-dialyzer({nowarn_function, execute/2}).
-spec execute(Req, Env) ->
                     {ok, Req, Env}|
                     {halt, Req}|
                     {error, StatusCode, Req} when
      Req :: cowboyku_req:req(),
      Env :: cowboyku_middleware:env(),
      StatusCode :: cowboyku:http_status().
execute(Req, Env) ->
    case midjan_core:start({Req, Env}, [{ordered, vegur_utils:config(middleware_stack)},
                                        {translator, vegur_midjan_translator},
                                        {finally, fun finally/1}
                                       ]) of
        {done, {halt, _Req} = Ret} ->
            Ret;
        {done, {error, _StatusCode, _Req} = Ret} ->
            Ret;
        {done, {Req1, Env1}} ->
            {ok, Req1, Env1}
    end.

finally(Return) ->
    %% Check the backend back in
    Req = case Return of
        {halt, Req0} -> Req0;
        {halt, _Code, Req0} -> Req0;
        {error, _Code, Req0} -> Req0
    end,
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    {DomainGroup, Req2} = cowboyku_req:meta(domain_group, Req1),
    {Service, Req3} = cowboyku_req:meta(service, Req2),
    Phase = case {DomainGroup, Service} of
        {undefined, undefined} -> lookup;
        {_, undefined} -> checkout;
        _ -> connected
    end,
    {ServiceState, Req4} = cowboyku_req:meta(service_state, Req3, normal),
    {ok, Req5, HandlerState2} = InterfaceModule:checkin_service(
        DomainGroup, Service, Phase, ServiceState, Req4, HandlerState
    ),
    ReqFinal = vegur_utils:set_handler_state(HandlerState2, Req5),
    %% Call the logger
    Final = case Return of
                {halt, _} -> {halt, ReqFinal};
                {halt, Code, _} -> {halt, Code, ReqFinal};
                {error, Code, _} -> {error, Code, ReqFinal}
            end,
    vegur_request_log:done(Final).
