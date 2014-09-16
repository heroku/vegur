-module(toy_router).
-behaviour(vegur_interface).
-export([init/2,
         terminate/3,
         lookup_domain_name/3,
         checkout_service/3,
         checkin_service/6,
         service_backend/3,
         feature/2,
         additional_headers/2,
         error_page/4]).

-record(state, {tries = [] :: list()}).

init(AcceptTime, Upstream) ->
    random:seed(AcceptTime), % RNGs require per-process seeding
    {ok, Upstream, #state{}}. % state initialization here.

lookup_domain_name(_AllDomains, Upstream, State) ->
    %% hardcoded values, we don't care about the domain
    Servers = [{1, {127,0,0,1}, 8081},
               {2, {127,0,0,1}, 8082}],
    {ok, Servers, Upstream, State}.

checkout_service(Servers, Upstream, State=#state{tries=Tried}) ->
    Available = Servers -- Tried,
    case Available of
        [] ->
            {error, all_blocked, Upstream, State};
        _ ->
            N = random:uniform(length(Available)),
            Pick = lists:nth(N, Available),
            {service, Pick, Upstream, State#state{tries=[Pick | Tried]}}
    end.

service_backend({_Id, IP, Port}, Upstream, State) ->
    %% extract the IP:PORT from the chosen server.
    {{IP, Port}, Upstream, State}.

checkin_service(_Servers, _Pick, _Phase, _ServState, Upstream, State) ->
    %% if we tracked total connections, we would decrement the counters here
    {ok, Upstream, State}.

feature(_WhoCares, State) ->
    {disabled, State}.

additional_headers(_Log, State) ->
    {[], State}.

error_page(all_blocked, _DomainGroup, Upstream, State) ->
    {{502, [], <<>>}, Upstream, State}; % Bad Gateway
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
error_page({upstream, invalid_transfer_encoding}, _DomainGroup, Upstream, HandlerState) ->
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
error_page(bad_request_header, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(_, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState}.

terminate(_, _, _) ->
    ok.
