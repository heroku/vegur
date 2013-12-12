%% @copyright Heroku (2013).
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc On Request Logging handler
%% @end
-module(hstub_log_hook).

-include("hstub_log.hrl").

-export([on_request/1]).

on_request(Req) ->
    {Method, Req2} =  cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {URL, Req4} = cowboy_req:url(Req3),
    {Headers, Req5} = cowboy_req:headers(Req4),
    ?INFO("~s ~s~nUrl: ~s~n~p",
          [Method, Path, URL, Headers]),
    %% Get a request ID
    {RequestId, Req6} = cowboy_req:header(hstub_app:config(request_id_name), Req5),
    {RequestId1, Req7} = get_or_validate_request_id(RequestId, Req6),
    cowboy_req:set_meta(request_id, RequestId1, Req7).

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
