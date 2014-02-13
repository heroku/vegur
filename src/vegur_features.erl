-module(vegur_features).
-export([set_features/2, get_features/1, lookup/2]).

-type enabled() :: deep_continue.
-type options() :: [enabled()].

-export_type([enabled/0, options/0]).

-spec set_features(options(), Req) -> Req when
    Req :: cowboy_req:req().
set_features(Opts, Req) ->
    cowboy_req:set_meta(features, Opts, Req).

-spec get_features(Req) -> {options(), Req} when
    Req :: cowboy_req:req().
get_features(Req) ->
    cowboy_req:meta(features, Req, []).

-spec lookup(enabled(), options()) -> enabled | disabled.
lookup(Name, List) ->
    case lists:member(Name, List) of
        true -> enabled;
        false -> disabled
    end.
