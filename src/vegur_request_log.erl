-module(vegur_request_log).

-include("vegur_log.hrl").

-define(LOGGER, vegur_req_log).

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
      Req :: cowboy_req:req().
new(Req) ->
    Now = os:timestamp(),
    {Method, Req2} =  cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {URL, Req4} = cowboy_req:url(Req3),
    {Headers, Req5} = cowboy_req:headers(Req4),
    ?INFO("~s ~s~nUrl: ~s~n~p", [Method, Path, URL, Headers]),
    %% Get a request ID
    {RequestId, Req6} = cowboy_req:header(vegur_app:config(request_id_name), Req5),
    {RequestId1, Req7} = get_or_validate_request_id(RequestId, Req6),
    Req8 = cowboy_req:set_meta(request_id, RequestId1, Req7),
    Log = ?LOGGER:new(Now),
    Log1 = ?LOGGER:stamp(accepted, Log),
    Req9 = cowboy_req:set_meta(logging, Log1, Req8),
    {InterfaceModule, _, Req10} = vegur_utils:get_interface_module(Req9),
    {ok, HandlerState} = InterfaceModule:init(Now, RequestId1),
    vegur_utils:set_handler_state(HandlerState, Req10).

-spec done(Code, Headers, Body, Req) -> Req when
      Code :: cowboy:http_status(),
      Headers :: [{iolist(), iolist()}]|[],
      Body :: binary(),
      Req :: cowboy_req:req().
done(_, _, _, Req) ->
    case cowboy_req:meta(logging, Req) of
        {undefined, Req1} ->
            % The request failed validation in Cowboy, this could happen if the request
            % doesn't have a Host header, or absolute-uri in the request.
            Req1;
        {_, Req1} ->
            % This is handled in the other done function - this one (a cowboy onresponse handler)
            % might be run too early since it runs when cowboy:reply is called, which happens
            % in vegur_proxy
            Req1
    end.

-spec done({error, Code, Req}|{halt, Req}) -> {error, Code, Req}|
                                              {halt, Req} when
      Code :: cowboy:http_status(),
      Req :: cowboy_req:req().
done({error, Code, Req}) ->
    Req1 = handle_terminate(Req),
    {error, Code, Req1};
done({halt, Req}) ->
    Req1 = handle_terminate(Req),
    {halt, Req1}.

-spec stamp(EventType, Req) -> Req when
      EventType :: event_type(),
      Req :: cowboy_req:req().
stamp(EventType, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    cowboy_req:set_meta(logging, ?LOGGER:stamp(EventType, Log), Req1).

-spec log(LogType, Fun, Req) -> {any(), Req} when
      LogType :: log_type(),
      Fun :: fun(() -> any()),
      Req :: cowboy_req:req().
log(EventType, Fun, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {Ret, NewLog} = ?LOGGER:log(EventType, Fun, Log),
    {Ret, cowboy_req:set_meta(logging, NewLog, Req1)}.

total_routing_time(Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {?LOGGER:timestamp_diff(accepted, pre_connect, Log), Req1}.

get_log_value(EventType, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {?LOGGER:event_duration(EventType, Log), Req1}.

get_or_validate_request_id(undefined, Req) ->
    {get_request_id(), Req};
get_or_validate_request_id(ReqId, Req) ->
    case erequest_id:validate(ReqId, request_max_length()) of
        valid ->
            {ReqId, Req};
        invalid ->
            {get_request_id(), Req}
    end.

get_request_id() ->
    {ok, ReqId} = erequest_id:create(),
    ReqId.

-spec request_max_length() -> pos_integer().
request_max_length() ->
    vegur_app:config(request_id_max_size).

handle_terminate(Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {RequestStatus, Req2} = cowboy_req:meta(status, Req1),
    Log1 = ?LOGGER:stamp(responded, Log),
    TotalTime = ?LOGGER:timestamp_diff(accepted, responded, Log1),
    RouteTime = ?LOGGER:timestamp_diff(accepted, pre_connect, Log1),
    ConnectTime = ?LOGGER:event_duration(connect_time, Log1),
    {RequestStatus, Req2} = cowboy_req:meta(status, Req1),
    {BytesSent, Req3} = cowboy_req:meta(bytes_sent, Req2),
    {BytesRecv, Req4} = cowboy_req:meta(bytes_recv, Req3),
    {InterfaceModule, HandlerState, Req5} = vegur_utils:get_interface_module(Req4),
    InterfaceModule:terminate(RequestStatus, [{total_time, TotalTime},
                                              {route_time, RouteTime},
                                              {connect_time, ConnectTime},
                                              {bytes_sent, BytesSent},
                                              {bytes_recv, BytesRecv}], HandlerState),
    Req5.
