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
      Req :: cowboy_req:req().
new(Req) ->
    Now = os:timestamp(),
    %% Get a request ID
    {RequestIdRaw, Req1} = cowboy_req:header(vegur_utils:config(request_id_name), Req),
    RequestId = erequest_id:ensure(RequestIdRaw,
                                   ?REQ_ID_MIN_LENGTH,
                                   ?REQ_ID_MAX_LENGTH),
    Req2 = cowboy_req:set_meta(request_id, RequestId, Req1),
    Log = ?LOGGER:new(Now),
    Log1 = ?LOGGER:stamp(accepted, Log),
    Req3 = cowboy_req:set_meta(logging, Log1, Req2),
    {InterfaceModule, _, Req4} = vegur_utils:get_interface_module(Req3),
    {ok, Req5, HandlerState} = InterfaceModule:init(Now, Req4),
    vegur_utils:set_handler_state(HandlerState, Req5).

handle_terminate(Code, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {RequestStatus, Req2} = cowboy_req:meta(status, Req1),
    Log1 = ?LOGGER:stamp(responded, Log),
    Req3 = cowboy_req:set_meta(logging, Log1, Req2),
    Req4 = cowboy_req:set_meta(response_code, Code, Req3),
    {InterfaceModule, HandlerState, Req5} = vegur_utils:get_interface_module(Req4),
    InterfaceModule:terminate(RequestStatus, Req5, HandlerState),
    Req5.

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
    Req1 = handle_terminate(Code, Req),
    {error, Code, Req1};
done({halt, Req}) ->
    Req1 = handle_terminate(0, Req),
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
