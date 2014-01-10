-module(vegur_proxy_middleware).

-behaviour(cowboy_middleware).
-include("vegur_log.hrl").
-export([execute/2]).

-record(state, { backend_client :: term()
                 ,env
               }).

execute(Req, Env) ->
    {Client, Req1} = cowboy_req:meta(backend_connection, Req),
    proxy(Req1, #state{backend_client = Client,
                       env = Env}).

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
        {error, Error} ->
            Req1 = get_error(Req, Error, State),
            {halt, Req1}
    end.

send_body_to_backend({Method, Header, Body, Path, Url}, Req,
                     #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:send_body(Method, Header, Body, Path, Url, Req, BackendClient) of
        {done, Req1, BackendClient1} ->
            read_backend_response(Req1, State#state{backend_client=BackendClient1});
        {error, Error} ->
            Req1 = get_error(Req, Error, State),
            {halt, Req1}
    end.

read_backend_response(Req, #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:read_backend_response(Req, BackendClient) of
        {ok, Code, RespHeaders, Req1, BackendClient1} ->
            handle_backend_response(Code, RespHeaders, Req1,
                                    State#state{backend_client=BackendClient1});
        {error, _Error} ->
            {error, 503, Req}
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
             #state{backend_client=BackendClient, env=Env}=State) ->
    case vegur_proxy:relay(Code, Headers, Req, BackendClient) of
        {ok, Req1, _Client1} ->
            {ok, Req1, Env};
        {error, Error, Req1} ->
            Req2 = get_error(Req1, Error, State),
            {halt, Req2}
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

get_error(Req, Error, #state{env=Env}) ->
    InterfaceModule = vegur_utils:get_interface_module(Env),
    {DomainGroup, Req1} = cowboy_req:meta(domain_group, Req),
    {HttpCode, ErrorHeaders, ErrorBody} = InterfaceModule:error_page(Error, DomainGroup),
    {ok, Req1} = cowboy_req:reply(HttpCode, ErrorHeaders, ErrorBody, Req),
    Req1.
