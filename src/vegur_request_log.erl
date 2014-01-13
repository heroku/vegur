-module(vegur_request_log).

-include("vegur_log.hrl").

-define(LOGGER, vegur_req_log).

-export([new/1,
         log/3,
         get_log_value/2,
         stamp/2,
         total_routing_time/1
        ]).

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
    Log1 = ?LOGGER:stamp(connection_accepted, Log),
    Req9 = cowboy_req:set_meta(logging, Log1, Req8),
    {InterfaceModule, _, Req10} = vegur_utils:get_interface_module(Req9),
    {ok, HandlerState} = InterfaceModule:init(Now, RequestId1),
    vegur_utils:set_handler_state(HandlerState, Req10).

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
    {?LOGGER:timestamp_diff(pre_connect, connection_accepted, Log),
     Req1}.

get_log_value(EventType, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    Rep = ?LOGGER:linear_report(fun(X) ->
                                        if
                                            X =:= EventType ->
                                                true;
                                            true ->
                                                false
                                        end
                                end, Log),
    {proplists:get_value("total", Rep), Req1}.

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
