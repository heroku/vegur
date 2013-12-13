-module(hstub_request_log).

-include("hstub_log.hrl").

-define(LOGGER, hstub_req_log).

-export([new/1,
         log/3
        ]).

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
    {RequestId, Req6} = cowboy_req:header(hstub_app:config(request_id_name), Req5),
    {RequestId1, Req7} = get_or_validate_request_id(RequestId, Req6),
    Req8 = cowboy_req:set_meta(request_id, RequestId1, Req7),
    cowboy_req:set_meta(logging, hstub_req_log:new(Now), Req8).

log(EventType, Fun, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {Ret, NewLog} = ?LOGGER:log(EventType, Fun, Log),
    {Ret, cowboy_req:set_meta(logger, NewLog, Req1)}.

get_or_validate_request_id(undefined, Req) ->
    {get_request_id(), Req};
get_or_validate_request_id(ReqId, Req) ->
    case erequest_id:validate(ReqId, hstub_app:config(request_id_max_size)) of
        valid ->
            {ReqId, Req};
        invalid ->
            {get_request_id(), Req}
    end.

get_request_id() ->
    {ok, ReqId} = erequest_id:create(),
    ReqId.
