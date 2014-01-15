-module(vegur_proxy_middleware).

-behaviour(cowboy_middleware).
-include("vegur_log.hrl").
-export([execute/2]).

-record(state, { backend_client :: term()
                 ,env
               }).

execute(Req, Env) ->
    {Client, Req1} = cowboy_req:meta(backend_connection, Req),
    case proxy(Req1, #state{backend_client = Client, env = Env}) of
        {ok, Req2, #state{backend_client=Client1, env=Env1}} ->
            BytesCounts = vegur_client:byte_counts(Client1),
            Req3 = cowboy_req:set_meta(bytes_sent, proplists:get_value(bytes_sent, BytesCounts), Req2),
            Req4 = cowboy_req:set_meta(bytes_recv, proplists:get_value(bytes_recv, BytesCounts), Req3),
            Req5 = cowboy_req:set_meta(request_status, successful, Req4),
            {ok, Req5, Env1};
        {error, _Blame, Reason, Req2} ->
            {ErrorMsg, Req3} = get_error(Reason, Req2),
            Req4 = render_error(ErrorMsg, Req3),
            Req5 = cowboy_req:set_meta(request_status, error, Req4),
            Req6 = vegur_utils:set_request_status(error, Req5),
            {halt, Req6}
    end.

proxy(Req, State) ->
    {BackendReq, Req1} = parse_request(Req),
    send_to_backend(BackendReq, Req1, State).

send_to_backend({Method, Header, Body, Path, Url}=Request, Req,
                #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:send_headers(Method, Header, Body, Path, Url, Req, BackendClient) of
        {done, Req1, BackendClient1} -> % headers sent
            send_body_to_backend(Request, Req1, State#state{backend_client=BackendClient1});
        {ok, Code, RespHeaders, BackendClient1} -> % request ended without body sent
            handle_backend_response(Code, RespHeaders, Req,
                                    State#state{backend_client=BackendClient1});
        {error, Blame, Error} ->
            {error, Blame, Error, Req}
    end.

send_body_to_backend({Method, Header, Body, Path, Url}, Req,
                     #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:send_body(Method, Header, Body, Path, Url, Req, BackendClient) of
        {done, Req1, BackendClient1} ->
            read_backend_response(Req1, State#state{backend_client=BackendClient1});
        {error, Blame, Error} ->
            {error, Blame, Error, Req}
    end.

read_backend_response(Req, #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:read_backend_response(Req, BackendClient) of
        {ok, Code, RespHeaders, Req1, BackendClient1} ->
            handle_backend_response(Code, RespHeaders, Req1,
                                    State#state{backend_client=BackendClient1});
        {error, Blame, Error} ->
            {error, Blame, Error, Req}
    end.

handle_backend_response(Code, RespHeaders, Req, State) ->
    {Type, Req2} = cowboy_req:meta(request_type, Req, []),
    case lists:sort(Type) of
        [] ->
            http_request(Code, RespHeaders, Req2, State);
        [upgrade] ->
            upgrade_request(Code, RespHeaders, Req2, State)
    end.

upgrade_request(101, Headers, Req, #state{backend_client=BackendClient,
                                          env=Env}) ->
    {done, Req1} = vegur_proxy:upgrade(Headers, Req, BackendClient),
    {ok, Req1, Env};
upgrade_request(Code, Headers, Req, State) ->
    http_request(Code, Headers, Req, State).

http_request(Code, Headers, Req,
             #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:relay(Code, Headers, Req, BackendClient) of
        {ok, Req1, BackendClient1} ->
            {ok, Req1, State#state{backend_client=BackendClient1}};
        {error, Blame, Error, Req1} ->
            {error, Blame, Error, Req1}
    end.

parse_request(Req) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {Host, Req4} = cowboy_req:host(Req3),
    {Port, Req5} = cowboy_req:port(Req4),
    {Headers, Req6} = cowboy_req:headers(Req5),
    Url = iolist_to_binary([Host, ":", integer_to_list(Port)]),
    %% We handle the request differently based on whether it's chunked,
    %% has a known length, or if it has no body at all.
    {Body, Req7} = 
        case cowboy_req:has_body(Req6) of
            true ->
                case cowboy_req:body_length(Req6) of
                    {undefined, Req8} ->
                        {{stream, chunked}, Req8};
                    {Length, Req8} ->
                        {{stream, Length}, Req8}
                end;
            false ->
                {<<>>, Req6}
        end,
    {Headers2, Req9} = add_proxy_headers(Headers, Req7),
    {{Method, Headers2, Body, Path, Url}, Req9}.

add_proxy_headers(Headers, Req) ->
    {Headers1, Req1} = add_request_id(Headers, Req),
    {Headers2, Req2} = add_forwarded(Headers1, Req1),
    {Headers3, Req3} = add_via(Headers2, Req2),
    {Headers4, Req4} = add_connect_time(Headers3, Req3),
    add_total_route_time(Headers4, Req4).

add_connect_time(Headers, Req) ->
    {Time, Req1} = vegur_request_log:get_log_value(connect_time, Req),
    {vegur_utils:add_or_replace_header(vegur_app:config(connect_time_header),
                                       integer_to_list(Time), Headers),
     Req1}.

add_total_route_time(Headers, Req) ->
    {Time, Req1} =
        case vegur_request_log:total_routing_time(Req) of
            {undefined, Req2} ->
                {"null", Req2};
            {Time1, Req2} ->
                {integer_to_list(Time1), Req2}
        end,
    {vegur_utils:add_or_replace_header(vegur_app:config(route_time_header),
                                       Time, Headers), Req1}.

add_request_id(Headers, Req) ->
    {RequestId, Req1} = cowboy_req:meta(request_id, Req),
    {vegur_utils:add_or_replace_header(vegur_app:config(request_id_name), RequestId, Headers),
     Req1}.

add_forwarded(Headers, Req) ->
    Transport = cowboy_req:get(transport, Req),
    {{PeerAddress, DestPort}, Req1} =
        case Transport:name() of
            proxy_protocol_tcp ->
                ProxySocket = cowboy_req:get(socket, Req),
                {ok, {{PeerIp, _}, {_, DestPort1}}} = Transport:proxyname(ProxySocket),
                {{PeerIp, DestPort1}, Req};
            _ ->
                {{PeerIp, _}, Req3} = cowboy_req:peer(Req),
                {Port, Req4} = cowboy_req:port(Req3),
                {{PeerIp, Port}, Req4}
        end,
    {Headers1, Req2} = vegur_utils:add_or_append_header(<<"x-forwarded-for">>, inet:ntoa(PeerAddress),
                                                        Headers, Req1),
    Headers2 =
        case DestPort of
            80 ->
                vegur_utils:add_or_replace_header(<<"x-forwarded-proto">>, <<"http">>, Headers1);
            443 ->
                vegur_utils:add_or_replace_header(<<"x-forwarded-proto">>, <<"https">>, Headers1);
            _ ->
                Headers1
        end,
    Headers3 = vegur_utils:add_or_replace_header(<<"x-forwarded-port">>, integer_to_list(DestPort), Headers2),
    {Headers3, Req2}.

add_via(Headers, Req) ->
    vegur_utils:add_or_append_header(<<"via">>, get_via_value(), Headers, Req).

-spec get_via_value() -> binary().
get_via_value() ->
    vegur_app:config(instance_name, <<"vegur">>).

get_error(Error, Req) ->
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    {DomainGroup, Req1} = cowboy_req:meta(domain_group, Req),
    {ErrorPage, HandlerState1} = InterfaceModule:error_page(Error, DomainGroup, HandlerState),
    Req2 = vegur_utils:set_handler_state(HandlerState1, Req1),
    {ErrorPage, Req2}.

render_error({HttpCode, ErrorHeaders, ErrorBody}, Req) ->
    vegur_utils:render_response(HttpCode, ErrorHeaders, ErrorBody, Req).
