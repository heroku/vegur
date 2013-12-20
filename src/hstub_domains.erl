-module(hstub_domains).

-export([lookup/1,
         in_maintenance_mode/1,
         route_id/1
        ]).

-record(domain_group, {domain :: domain(),
                       maintenance_mode = false :: boolean()}).
-opaque domain_group() :: #domain_group{}.
-type domain() :: binary().
-export_type([domain_group/0
              ,domain/0
             ]).

-spec lookup(binary()) ->
                    undefined |
                    {error, {herokuapp, domain_group(), domain()}} |
                    {ok, domain(), domain_group()}.
lookup(Domain) ->
    case hstub_app:config(domain) of
        Domain ->
            {ok, Domain, #domain_group{domain=Domain}};
        _ ->
            undefined
    end.

-spec route_id(domain_group()) ->
                      undefined | binary().
route_id(#domain_group{}) ->
    <<"group_id">>.

-spec in_maintenance_mode(domain_group()) ->
                                 true | false.
in_maintenance_mode(#domain_group{maintenance_mode=MaintenanceMode}) ->
    %% Would look into the record and return true or false
    MaintenanceMode.
