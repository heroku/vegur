-module(hstub_domains).

-export([lookup/1,
         in_maintenance_mode/1,
         route_id/1
        ]).

-opaque domain_group() :: term().
-export_type([domain_group/0]).

-spec lookup(binary()) -> ok.
lookup(_Domain) ->
    %% STUB for mocking.
    ok.

-spec route_id(domain_group()) ->
                      undefined | binary().
route_id(_DomainGroup) ->
    ok.

-spec in_maintenance_mode(domain_group()) ->
                                  true | false.
in_maintenance_mode(_DomainGroup) ->
    %% Would look into the record and return true or false
    ok.
