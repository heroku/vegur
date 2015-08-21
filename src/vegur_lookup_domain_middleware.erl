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
-module(vegur_lookup_domain_middleware).

-behaviour(cowboyku_middleware).
-export([execute/2]).

execute(Req, Env) ->
    {Host, Req1} = cowboyku_req:host(Req),
    {InterfaceModule, HandlerState, Req2} = vegur_utils:get_interface_module(Req1),
    case InterfaceModule:lookup_domain_name(Host, Req2, HandlerState) of 
        {error, Reason, Req3, HandlerState1} ->
            Req4 = vegur_utils:set_handler_state(HandlerState1, Req3),
            handle_error(Reason, Req4, Env);
        {redirect, Reason, DomainGroup, Domain, Req3, HandlerState1} ->
            Req4 = vegur_utils:set_handler_state(HandlerState1, Req3),
            handle_redirect(Reason, DomainGroup, Domain, Req4, Env);
        {ok, DomainGroup, Req3, HandlerState1} ->
            Req4 = vegur_utils:set_handler_state(HandlerState1, Req3),
            Req5 = cowboyku_req:set_meta(domain_group, DomainGroup, Req4),
            {ok, Req5, Env}
    end.

-spec handle_error(Reason, Req, Env) -> 
                          {error, HttpCode, Req} when
      Reason :: atom(),
      Req :: cowboyku_req:req(),
      Env :: cowboyku_middleware:env(),
      HttpCode :: cowboyku:http_status().
handle_error(Reason, Req, _Env) ->
    {HttpCode, Req1} = vegur_utils:handle_error(Reason, Req),
    {error, HttpCode, Req1}.

-spec handle_redirect(Reason, DomainGroup, Domain, Req, Env) ->
                             {halt, HttpCode, Req} when
      Reason :: any(),
      DomainGroup :: vegur_interface:domain_group(),
      Domain :: vegur_interface:domain(),
      Req :: cowboyku_req:req(),
      HttpCode :: cowboyku:http_status(),
      Env :: cowboyku_middleware:env().
handle_redirect(_Reason, _DomainGroup, RedirectTo, Req, _Env) ->
    {FullLocation, Req2} = build_redirect_uri(RedirectTo, Req),
    {ok, Req3} = cowboyku_req:reply(301, [{<<"location">>, FullLocation}], Req2),
    {halt, 301, Req3}.

% Internal

build_redirect_uri(RedirectTo, Req) ->
    {Path, Req1} = cowboyku_req:path(Req),
    {Qs, Req2} = get_querystring(Req1),
    {HeaderValue, Req3} = cowboyku_req:header(<<"x-forwarded-proto">>, Req2),
    Proto = get_proto(HeaderValue),
    {[Proto, <<"://">>, RedirectTo, Path, Qs], Req3}.

get_querystring(Req) ->
    {Qs, Req2} = cowboyku_req:qs(Req),
    case Qs of
        <<>> -> {<<>>, Req2};
        _ -> {["?", Qs], Req2}
    end.

get_proto(<<"https">>) ->
    <<"https">>;
get_proto(_) ->
    <<"http">>.

