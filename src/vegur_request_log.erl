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
-module(vegur_request_log).

-define(LOGGER, vegur_req_log).

-define(REQ_ID_MIN_LENGTH, 20).
-define(REQ_ID_MAX_LENGTH, 200).

-export([new/1,
         done/1,
         done/4,
         log/3,
         get_log_value/2,
         stamp/2,
         total_routing_time/1]).

-type event_type() :: pre_connect|connection_accepted.
-type log_type() :: domain_lookup|service_lookup|connect_time.

-export_type([event_type/0,
              log_type/0]).

-spec new(Req) -> Req when
      Req :: cowboyku_req:req().
new(Req) ->
    Now = os:timestamp(),
    %% Get a request ID
    {RequestIdRaw, Req1} = cowboyku_req:header(vegur_utils:config(request_id_name), Req),
    RequestId = erequest_id:ensure(RequestIdRaw,
                                   ?REQ_ID_MIN_LENGTH,
                                   ?REQ_ID_MAX_LENGTH),
    Req2 = cowboyku_req:set_meta(request_id, RequestId, Req1),
    Log = ?LOGGER:new(Now),
    Log1 = ?LOGGER:stamp(accepted, Log),
    Req3 = cowboyku_req:set_meta(logging, Log1, Req2),
    {InterfaceModule, _, Req4} = vegur_utils:get_interface_module(Req3),
    {ok, Req5, HandlerState} = InterfaceModule:init(Now, Req4),
    vegur_utils:set_handler_state(HandlerState, Req5).

handle_terminate(Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    {RequestStatus, Req2} = cowboyku_req:meta(status, Req1),
    Log1 = ?LOGGER:stamp(responded, Log),
    Req3 = cowboyku_req:set_meta(logging, Log1, Req2),
    {InterfaceModule, HandlerState, Req4} = vegur_utils:get_interface_module(Req3),
    InterfaceModule:terminate(RequestStatus, Req4, HandlerState),
    Req4.

-spec done(Code, Headers, Body, Req) -> Req when
      Code :: cowboyku:http_status(),
      Headers :: [{iolist(), iolist()}]|[],
      Body :: binary(),
      Req :: cowboyku_req:req().
done(_, _, _, Req) ->
    case cowboyku_req:meta(logging, Req) of
        {undefined, Req1} ->
            % The request failed validation in Cowboyku, this could happen if the request
            % doesn't have a Host header, or absolute-uri in the request.
            Req1;
        {_, Req1} ->
            % This is handled in the other done function - this one (a cowboyku onresponse handler)
            % might be run too early since it runs when cowboyku:reply is called, which happens
            % in vegur_proxy
            Req1
    end.

-spec done({error, Code, Req}|{halt, Code, Req}|{halt, Req}) ->
    {error, Code, Req}|{halt, Req} when
      Code :: cowboyku:http_status(),
      Req :: cowboyku_req:req().
done({error, Code, Req}) ->
    Req1 = cowboyku_req:set_meta(response_code, Code, Req),
    Req2 = handle_terminate(Req1),
    {error, Code, Req2};
done({halt, Code, Req}) ->
    Req1 = cowboyku_req:set_meta(response_code, Code, Req),
    Req2 = handle_terminate(Req1),
    {halt, Req2}; % don't pass on the code, unsupported by cowboyku
done({halt, Req}) ->
    Req1 = handle_terminate(Req),
    {halt, Req1}.

-spec stamp(EventType, Req) -> Req when
      EventType :: event_type(),
      Req :: cowboyku_req:req().
stamp(EventType, Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    cowboyku_req:set_meta(logging, ?LOGGER:stamp(EventType, Log), Req1).

-spec log(LogType, Fun, Req) -> {any(), Req} when
      LogType :: log_type(),
      Fun :: fun(() -> any()),
      Req :: cowboyku_req:req().
log(EventType, Fun, Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    {Ret, NewLog} = ?LOGGER:log(EventType, Fun, Log),
    {Ret, cowboyku_req:set_meta(logging, NewLog, Req1)}.

total_routing_time(Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    {?LOGGER:timestamp_diff(accepted, pre_connect, Log), Req1}.

get_log_value(EventType, Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    {?LOGGER:event_duration(EventType, Log), Req1}.
