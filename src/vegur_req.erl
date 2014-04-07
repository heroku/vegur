%%%-------------------------------------------------------------------
%% @copyright Heroku (2014)
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
         ,pre_connect/1
         ,pre_proxy/1
         ,bytes_recv/1
         ,bytes_sent/1
         ,method/1
         ,path/1
         ,header/2
         ,response_code/1
        ]).

-spec start_time(Req) -> {StartTime, Req} when
      Req :: cowboy_req:req(),
      StartTime :: erlang:timestamp().
start_time(Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {vegur_req_log:start_time(Log), Req1}.

-spec start_to_proxy_duration(Req) -> {StartToProxyDuration, Req} when
      Req :: cowboy_req:req(),
      StartToProxyDuration :: erlang:timestamp().
start_to_proxy_duration(Req) ->
    {StartTime, Req1} = start_time(Req),
    {PreProxy, Req2} = pre_proxy(Req1),
    {timer:now_diff(PreProxy, StartTime) div 1000, Req2}.

-spec request_id(Req) -> {RequestId, Req} when
      Req :: cowboy_req:req(),
      RequestId :: erequest_id:request_id().
request_id(Req) ->
    cowboy_req:meta(request_id, Req, <<>>).

-spec peer(Req) -> {Peer, Req} when
      Peer :: {inet:ip_address(), inet:port_number()},
      Req :: cowboy_req:req().
peer(Req) ->
    cowboy_req:peer(Req).

-spec proxy_peer(Req) -> {Peer, Req} when
      Peer :: {inet:ip_address(), inet:port_number()},
      Req :: cowboy_req:req().
proxy_peer(Req) ->
    case vegur_utils:peer_ip_port(Req) of
        {{PeerIp, _PeerPort, DestPort}, Req} -> {{PeerIp, DestPort}, Req};
        Tuple -> Tuple
    end.

-spec pre_connect(Req) -> {PreConnectTime|undefined, Req} when
      PreConnectTime :: erlang:timestamp(),
      Req :: cowboy_req:req().
pre_connect(Req) ->
    event(pre_connect, Req).

-spec pre_proxy(Req) -> {PreProxyTime|undefined, Req} when
      PreProxyTime :: erlang:timestamp(),
      Req :: cowboy_req:req().
pre_proxy(Req) ->
    event(pre_proxy, Req).

-spec route_duration(Req) -> {RouteTime|undefined, Req} when
      RouteTime :: non_neg_integer(),
      Req :: cowboy_req:req().
route_duration(Req) ->
    timestamp_diff(accepted, pre_connect, Req).

-spec connect_duration(Req) -> {ConnectTime|undefined, Req} when
      ConnectTime :: non_neg_integer(),
      Req :: cowboy_req:req().
connect_duration(Req) ->
    duration(connect_time, Req).

-spec service_duration(Req) -> {ServiceTime|undefined, Req} when
      ServiceTime :: non_neg_integer(),
      Req :: cowboy_req:req().
service_duration(Req) ->
    duration(service_time, Req).

-spec total_duration(Req) -> {TotalTime|undefined, Req} when
      TotalTime :: non_neg_integer(),
      Req :: cowboy_req:req().
total_duration(Req) ->
    timestamp_diff(accepted, responded, Req).

-spec bytes_recv(Req) -> {BytesRecv|undefined, Req} when
      BytesRecv :: non_neg_integer(),
      Req :: cowboy_req:req().
bytes_recv(Req) ->
    cowboy_req:meta(bytes_recv, Req, undefined).

-spec bytes_sent(Req) -> {BytesSent|undefined, Req} when
      BytesSent :: non_neg_integer(),
      Req :: cowboy_req:req().
bytes_sent(Req) ->
    cowboy_req:meta(bytes_sent, Req, undefined).

-spec method(Req) -> {Method, Req} when
      Method :: binary(),
      Req :: cowboy_req:req().
method(Req) ->
    cowboy_req:method(Req).

-spec path(Req) -> {Path, Req} when
      Path :: binary(),
      Req :: cowboy_req:req().
path(Req) ->
    cowboy_req:path(Req).

-spec header(Key, Req) -> {Value|undefined, Req} when
      Key :: binary(),
      Value :: binary()|undefined,
      Req :: cowboy_req:req().
header(Key, Req) ->
    cowboy_req:header(Key, Req).

-spec response_code(Req) -> {Code, Req} when
      Req :: cowboy_req:req(),
      Code :: non_neg_integer().
response_code(Req) ->
    cowboy_req:meta(response_code, Req, <<>>).

%% Internal
timestamp_diff(FromKey, ToKey, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {vegur_req_log:timestamp_diff(FromKey, ToKey, Log), Req1}.

duration(Key, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {vegur_req_log:event_duration(Key, Log), Req1}.

event(Key, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {vegur_req_log:event(Key, Log), Req1}.
