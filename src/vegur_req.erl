%%%-------------------------------------------------------------------
%% @copyright Heroku (2013)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc vegur req public API
%% @end
%%%-------------------------------------------------------------------

-module(vegur_req).

%% API
-export([request_id/1
         ,peer/1
         ,proxy_peer/1
         ,route_time/1
         ,connect_time/1
         ,service_time/1
         ,total_time/1
         ,bytes_recv/1
         ,bytes_sent/1
         ,method/1
         ,path/1
         ,header/2
        ]).

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
    vegur_utils:peer_ip_port(Req).

-spec route_time(Req) -> {RouteTime|undefined, Req} when
      RouteTime :: non_neg_integer(),
      Req :: cowboy_req:req().
route_time(Req) ->
    timestamp_diff(accepted, pre_connect, Req).

-spec connect_time(Req) -> {ConnectTime|undefined, Req} when
      ConnectTime :: non_neg_integer(),
      Req :: cowboy_req:req().
connect_time(Req) ->
    duration(connect_time, Req).

-spec service_time(Req) -> {ServiceTime|undefined, Req} when
      ServiceTime :: non_neg_integer(),
      Req :: cowboy_req:req().
service_time(Req) ->
    duration(service_time, Req).

-spec total_time(Req) -> {TotalTime|undefined, Req} when
      TotalTime :: non_neg_integer(),
      Req :: cowboy_req:req().
total_time(Req) ->
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

%% Internal
timestamp_diff(FromKey, ToKey, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {vegur_req_log:timestamp_diff(FromKey, ToKey, Log), Req1}.

duration(Key, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {vegur_req_log:event_duration(Key, Log), Req1}.
