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
    Req5.
