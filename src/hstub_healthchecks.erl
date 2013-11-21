-module(hstub_healthchecks).

-export([accepting_connections/0,
         lockstep_fresh/0]).

accepting_connections() ->
    case hstub_app:config(proxy_deny, false) of
        false ->
            true;
        _ ->
            false
    end.

lockstep_fresh() ->
    case hstub_app:config(lockstep_fresh, true) of
        true ->
            true;
        _ ->
            false
    end.
