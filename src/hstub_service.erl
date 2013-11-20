-module(hstub_service).

-export([lookup/1]).

-opaque service() :: record().
-export_type([service/0]).

lookup(DomainGroup) ->
    case hstub_domains:route_id(DomainGroup) of
        undefined ->
            {error, no_route_id};
        _ ->
            %% STUB for lookup code. Here we are to call hermes_route, and do some
            %% cleanup to work with the limitations of the spec of this function.
            ok
    end.
                  
