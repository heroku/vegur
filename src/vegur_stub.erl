-module(vegur_stub).

-behaviour(vegur_interface).

-export([init/2,
         terminate/3,
         lookup_domain_name/3,
         checkout_service/3,
         checkin_service/6,
         service_backend/3,
         feature/2,
         error_page/4]).

-record(state, {
          connect_tries = 0 :: non_neg_integer()
         }).


-spec init(RequestAccepted, Upstream) ->
                  {ok, Upstream, HandlerState} when
      RequestAccepted :: erlang:timestamp(),
      Upstream :: vegur_interface:upstream(),
      HandlerState :: vegur_interface:handler_state().
init(_, Upstream) ->
    {ok, Upstream, #state{}}.

-spec lookup_domain_name(Domain, Upstream, HandlerState) ->
                                {error, not_found, Upstream, HandlerState} |
                                {redirect, Reason, DomainGroup, Domain, Upstream, HandlerState} |
                                {ok, DomainGroup, Upstream, HandlerState} when
      Domain :: vegur_interface:domain(),
      Reason :: atom(),
      DomainGroup :: vegur_interface:domain_group(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
lookup_domain_name(_Domain, Upstream, HandlerState) ->
    {ok, domain_group, Upstream, HandlerState}.

-spec checkout_service(DomainGroup, Upstream, HandlerState) ->
                              {service, Service, Upstream, HandlerState} |
                              {error, CheckoutError, Upstream, HandlerState} when
      CheckoutError :: atom(),
      DomainGroup :: vegur_interface:domain_group(),
      Service :: vegur_interface:service(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
checkout_service(_App, Connection, #state{connect_tries=10}=HandlerState) ->
    {error, econn_refused, Connection, HandlerState};
checkout_service(maintenance_mode, Connection, HandlerState) ->
    {error, maintenance_mode, Connection, HandlerState};
checkout_service(_DomainGroup, Upstream, HandlerState) ->
    {service, service, Upstream, HandlerState}.

-spec checkin_service(DomainGroup, Service, Phase, ServiceState, Upstream, HandlerState) ->
                             {ok, Upstream, HandlerState} when
      DomainGroup :: vegur_interface:domain_group(),
      Service :: vegur_interface:service(),
      Phase :: vegur_interface:phase(),
      ServiceState :: vegur_interface:service_state(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
checkin_service(_DomainGroup, _Service, _Phase, _ServiceState, Upstream, HandlerState=#state{connect_tries=Tries}) ->
    {ok, Upstream, HandlerState#state{connect_tries=Tries+1}}.

-spec feature(Feature, HandlerState) -> {enabled | disabled, HandlerState} when
      Feature :: vegur_interface:feature(),
      HandlerState :: vegur_interface:handler_state().
feature(deep_continue, State) ->
    {enabled, State};
feature(peer_port, State) ->
    {disabled, State};
feature(_, State) ->
    {disabled, State}.

-spec error_page(ErrorReason, DomainGroup, Upstream, HandlerState) ->
                        {{HttpCode, ErrorHeaders, ErrorBody}, Upstream, HandlerState} when
      ErrorReason :: term(),
      DomainGroup :: vegur_interface:domain_group(),
      HttpCode :: pos_integer(),
      ErrorHeaders :: [{iolist(), iolist()}]|[],
      ErrorBody :: binary(),
      HandlerState :: vegur_interface:handler_state(),
      Upstream :: vegur_interface:upstream().
%% Example errors that could be returned by this module and end up in here,
%% and can also be used by the existing test suite.
error_page(not_found, _DomainGroup, Upstream, HandlerState) ->
    {{404, [], <<>>}, Upstream, HandlerState};
error_page(no_route_id, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({backlog_timeout, _QueueLen, _WaitTime}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({backlog_too_deep, _QueueLen, _WaitTime}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({conn_limit_reached, _QueueLen, _WaitTime}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(route_lookup_failed, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(no_web_processes, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(crashed, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(backends_quarantined, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(backends_starting, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(backends_idle, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(app_not_found, _DomainGroup, Upstream, HandlerState) ->
    {{404, [], <<>>}, Upstream, HandlerState};
error_page(app_lookup_failed, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(maintainance_mode, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(econn_timeout, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(econn_refused, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page(app_blank, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({app_blank, _App}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
%% Vegur-returned errors that should be handled no matter what
error_page(expectation_failed, _DomainGroup, Upstream, HandlerState) ->
    {{417, [], <<>>}, Upstream, HandlerState};
error_page({upstream, closed}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, closed}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, timeout}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({upstream, timeout}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({undefined, timeout}, _DomainGroup, Upstream, HandlerState) ->
    %% Who knows who timed out. Technically both!
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, invalid_status}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, content_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, cookie_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, header_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, status_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({upstream, {bad_chunk,_}}, _DomainGroup, Upstream, HandlerState) ->
    %% bad chunked encoding from client
    {{400, [], <<>>}, Upstream, HandlerState};
error_page({upstream, unexpected_data}, _DomainGroup, Upstream, HandlerState) ->
    %% pipelined request (maybe), not supported.
    {{400, [], <<>>}, Upstream, HandlerState};
error_page({downstream, {bad_chunk,_}}, _DomainGroup, Upstream, HandlerState) ->
    %% bad chunked encoding from server
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, non_terminal_status_after_continue}, _DomainGroup, Upstream, HandlerState) ->
    %% Can't send a 1xx status after a 100 continue (except for upgrades)
    %% when expect: 100-continue is declared
    {{502, [], <<>>}, Upstream, HandlerState};
%% Generic handling
error_page(empty_host, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(_, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState}.

-spec service_backend(Service, Upstream, HandlerState) ->
                             {ServiceBackend, Upstream, HandlerState} when
      Service :: vegur_interface:service(),
      HandlerState :: vegur_interface:handler_state(),
      ServiceBackend :: vegur_interface:service_backend(),
      Upstream :: vegur_interface:upstream().
service_backend(_Service, Upstream, HandlerState) ->
    {{{127,0,0,1}, 80}, Upstream, HandlerState}.

-spec terminate(Status, Upstream, HandlerState) -> ok when
      Status :: vegur_interface:terminate_reason(),
      Upstream :: vegur_interface:upstream(),
      HandlerState :: vegur_interface:handler_state().
terminate(_, _, _) ->
    ok.
