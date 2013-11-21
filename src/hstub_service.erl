-module(hstub_service).

-export([lookup/1]).

-opaque service() :: record().
-type queue_length() :: pos_integer().
-type wait_time() :: pos_integer().
-type service_lookup_result() :: {route, hstub_services:service()} |
                                 {error, no_route_id} |
                                 {error, {backlog_timeout, queue_length(), wait_time()}} |
                                 {error, {backlog_too_deep, queue_length(), wait_time()}} |
                                 {error, {conn_limit_reached, queue_length(), wait_time()}} |
                                 {error, route_lookup_failed} |
                                 {error, no_web_processes} |
                                 {error, crashed} |
                                 {error, backends_quarantined} |
                                 {error, backends_starting} |
                                 {error, backends_idle} |
                                 {error, app_blank} |
                                 {error, app_not_found} |
                                 {error, app_lookup_failed}.
-export_type([service/0
              ,queue_length/0
              ,wait_time/0
              ,service_lookup_result/0
             ]).

lookup(DomainGroup) ->
    case hstub_domains:route_id(DomainGroup) of
        undefined ->
            {error, no_route_id};
        _ ->
            %% STUB for lookup code. Here we are to call hermes_route, and do some
            %% cleanup to work with the limitations of the spec of this function.
            ok
    end.
                  
