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
%%%-------------------------------------------------------------------
%% @copyright Heroku (2014-2015)
%% @author Omar Yasin <omarkj@heroku.com>
%% @version {@vsn}, {@date} {@time}
%% @doc vegur req public API
%% @end
%%%-------------------------------------------------------------------

-module(vegur_req).

%% API
-export([start_time/1
         ,start_to_proxy_duration/1
         ,request_id/1
         ,peer/1
         ,proxy_peer/1
         ,route_duration/1
         ,connect_duration/1
         ,service_duration/1
         ,total_duration/1
         ,send_headers_duration/1
         ,request_proxy_duration/1
         ,response_proxy_duration/1
         ,pre_connect/1
         ,pre_proxy/1
         ,bytes_recv/1
         ,bytes_sent/1
         ,method/1
         ,path/1
         ,raw_path/1
         ,header/2
         ,response_code/1
         ,response_headers/1
         ,connection_info/1
         ,connection_info/2
        ]).

-spec start_time(Req) -> {StartTime, Req} when
      Req :: cowboyku_req:req(),
      StartTime :: erlang:timestamp().
start_time(Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    {vegur_req_log:start_time(Log), Req1}.

-spec start_to_proxy_duration(Req) -> {StartToProxyDuration, Req} when
      Req :: cowboyku_req:req(),
      StartToProxyDuration :: integer().
start_to_proxy_duration(Req) ->
    {StartTime, Req1} = start_time(Req),
    {PreProxy, Req2} = pre_proxy(Req1),
    {timer:now_diff(PreProxy, StartTime) div 1000, Req2}.

-spec request_id(Req) -> {RequestId, Req} when
      Req :: cowboyku_req:req(),
      RequestId :: erequest_id:request_id().
request_id(Req) ->
    cowboyku_req:meta(request_id, Req, <<>>).

-spec peer(Req) -> {Peer, Req} when
      Peer :: {inet:ip_address(), inet:port_number()},
      Req :: cowboyku_req:req().
peer(Req) ->
    cowboyku_req:peer(Req).

-spec proxy_peer(Req) -> {Peer, Req} when
      Peer :: {inet:ip_address(), inet:port_number()},
      Req :: cowboyku_req:req().
proxy_peer(Req) ->
    case vegur_utils:peer_ip_port(Req) of
        {{PeerIp, _PeerPort, DestPort}, Req} -> {{PeerIp, DestPort}, Req};
        Tuple -> Tuple
    end.

-spec pre_connect(Req) -> {PreConnectTime|undefined, Req} when
      PreConnectTime :: erlang:timestamp(),
      Req :: cowboyku_req:req().
pre_connect(Req) ->
    event(pre_connect, Req).

-spec pre_proxy(Req) -> {PreProxyTime|undefined, Req} when
      PreProxyTime :: erlang:timestamp(),
      Req :: cowboyku_req:req().
pre_proxy(Req) ->
    event(pre_proxy, Req).

-spec route_duration(Req) -> {RouteTime|undefined, Req} when
      RouteTime :: non_neg_integer(),
      Req :: cowboyku_req:req().
route_duration(Req) ->
    timestamp_diff(accepted, pre_connect, Req).

-spec connect_duration(Req) -> {ConnectTime|undefined, Req} when
      ConnectTime :: non_neg_integer(),
      Req :: cowboyku_req:req().
connect_duration(Req) ->
    duration(connect_time, Req).

-spec service_duration(Req) -> {ServiceTime|undefined, Req} when
      ServiceTime :: non_neg_integer(),
      Req :: cowboyku_req:req().
service_duration(Req) ->
    duration(service_time, Req).

-spec total_duration(Req) -> {TotalTime|undefined, Req} when
      TotalTime :: non_neg_integer(),
      Req :: cowboyku_req:req().
total_duration(Req) ->
    timestamp_diff(accepted, responded, Req).

-spec send_headers_duration(Req) -> {TotalTime|undefined, Req} when
      TotalTime :: non_neg_integer(),
      Req :: cowboyku_req:req().
send_headers_duration(Req) ->
    timestamp_diff(headers_formatted, headers_sent, Req).

-spec request_proxy_duration(Req) -> {TotalTime|undefined, Req} when
      TotalTime :: non_neg_integer(),
      Req :: cowboyku_req:req().
request_proxy_duration(Req) ->
    timestamp_diff(headers_sent, client_last_packet_sent, Req).

-spec response_proxy_duration(Req) -> {TotalTime|undefined, Req} when
      TotalTime :: non_neg_integer(),
      Req :: cowboyku_req:req().
response_proxy_duration(Req) ->
    timestamp_diff(client_first_packet_recv, client_last_packet_recv, Req).

-spec bytes_recv(Req) -> {BytesRecv|undefined, Req} when
      BytesRecv :: non_neg_integer(),
      Req :: cowboyku_req:req().
bytes_recv(Req) ->
    cowboyku_req:meta(bytes_recv, Req, undefined).

-spec bytes_sent(Req) -> {BytesSent|undefined, Req} when
      BytesSent :: non_neg_integer(),
      Req :: cowboyku_req:req().
bytes_sent(Req) ->
    cowboyku_req:meta(bytes_sent, Req, undefined).

-spec method(Req) -> {Method, Req} when
      Method :: binary(),
      Req :: cowboyku_req:req().
method(Req) ->
    cowboyku_req:method(Req).

-spec path(Req) -> {Path, Req} when
      Path :: binary(),
      Req :: cowboyku_req:req().
path(Req) ->
    cowboyku_req:path(Req).

-spec raw_path(Req) -> {RawPath, Req} when
    RawPath :: binary(),
    Req :: cowboyku_req:req().
raw_path(Req) ->
    {Path, Req1} = cowboyku_req:path(Req),
    {Qs, Req2} = cowboyku_req:qs(Req1),
    RawPath = case Qs of
        <<>> -> Path;
        _ -> <<Path/binary, "?", Qs/binary>>
    end,
    {RawPath, Req2}.

-spec header(Key, Req) -> {Value|undefined, Req} when
      Key :: binary(),
      Value :: binary()|undefined,
      Req :: cowboyku_req:req().
header(Key, Req) ->
    cowboyku_req:header(Key, Req).

-spec response_code(Req) -> {Code, Req} when
      Req :: cowboyku_req:req(),
      Code :: non_neg_integer().
response_code(Req) ->
    cowboyku_req:meta(response_code, Req, <<>>).

-spec response_headers(Req) -> {[{iodata(), iodata()}], Req} when
    Req :: cowboyku_req:req().
response_headers(Req) ->
    cowboyku_req:meta(response_headers, Req, []).

-spec connection_info(Req) -> {list(), Req} when
      Req :: cowboyku_req:req().
connection_info(Req) ->
    vegur_utils:connection_info(Req).

-spec connection_info([protocol | cipher_suite | sni_hostname], Req) -> {list(), Req} when
      Req :: cowboyku_req:req().
connection_info(Items, Req) ->
    vegur_utils:connection_info(Items, Req).

%% Internal
timestamp_diff(FromKey, ToKey, Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    {vegur_req_log:timestamp_diff(FromKey, ToKey, Log), Req1}.

duration(Key, Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    {vegur_req_log:event_duration(Key, Log), Req1}.

event(Key, Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    {vegur_req_log:event(Key, Log), Req1}.
