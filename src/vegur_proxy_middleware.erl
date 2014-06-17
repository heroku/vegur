-module(vegur_proxy_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

-record(state, { backend_client :: vegur_client:client()
                 ,env
               }).

execute(Req, Env) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    Log1 = vegur_req_log:stamp(pre_proxy, Log),
    Req2 = cowboy_req:set_meta(logging, Log1, Req1),
    {Client, Req3} = cowboy_req:meta(backend_connection, Req2),
    case vegur_req_log:log(service_time,
                           fun() ->
                                   proxy(Req3, #state{backend_client = Client, env = Env})
                           end, Log1) of
        {{ok, Code, _Status, Req4, #state{backend_client=Client1}}, Log2} ->
            Req5 = store_byte_counts(Req4, Client1),
            Req6 = cowboy_req:set_meta(status, successful, Req5),
            Req7 = cowboy_req:set_meta(logging, Log2, Req6),
            {halt, Code, Req7};
        {{error, Blame, Reason, Req4}, Log2} ->
            {HttpCode, Req5} = vegur_utils:handle_error({Blame, Reason}, Req4),
            Req6 = store_byte_counts(Req5, Client),
            Req7 = cowboy_req:set_meta(logging, Log2, Req6),
            {error, HttpCode, Req7}
    end.

proxy(Req, State) ->
    case parse_request(Req) of
        {BackendReq, Req1} ->
            send_to_backend(BackendReq, Req1, State);
        {error, Blame, Error} ->
            {error, Blame, Error, Req}
    end.

send_to_backend({Method, Header, Body, Path, Url}=Request, Req,
                #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:send_headers(Method, Header, Body, Path, Url, Req, BackendClient) of
        {done, Req1, BackendClient1} -> % headers sent
            send_body_to_backend(Request, Req1, State#state{backend_client=BackendClient1});
        {ok, Code, Status, RespHeaders, BackendClient1} -> % request ended without body sent
            handle_backend_response(Code, Status, RespHeaders, Req,
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
            {error, Blame, Error, store_byte_counts(Req, BackendClient)}
    end.

read_backend_response(Req, #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:read_backend_response(Req, BackendClient) of
        {ok, Code, Status, RespHeaders, Req1, BackendClient1} ->
            handle_backend_response(Code, Status, RespHeaders, Req1,
                                    State#state{backend_client=BackendClient1});
        {error, Blame, Error} ->
            {error, Blame, Error, store_byte_counts(Req, BackendClient)}
    end.

handle_backend_response(Code, Status, RespHeaders, Req, State) ->
    {Type, Req2} = cowboy_req:meta(request_type, Req, []),
    case lists:sort(Type) of
        [] ->
            http_request(Code, Status, RespHeaders, Req2, State);
        [upgrade] ->
            upgrade_request(Code, Status, RespHeaders, Req2, State)
    end.

upgrade_request(101, Status, Headers, Req, #state{backend_client=BackendClient}=State) ->
    {Result, Req1, BackendClient1} = vegur_proxy:upgrade(Headers, Req, BackendClient),
    case Result of
        timeout ->
            {error, undefined, timeout, store_byte_counts(Req, BackendClient1)};
        done ->
            {ok, 101, Status, Req1, State#state{backend_client=BackendClient1}}
    end;
upgrade_request(Code, Status, Headers, Req, State) ->
    http_request(Code, Status, Headers, Req, State).

http_request(Code, Status, Headers, Req,
             #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:relay(Code, Status, Headers, Req, BackendClient) of
        {ok, Req1, BackendClient1} ->
            {ok, Code, Status, Req1, State#state{backend_client=BackendClient1}};
        {error, Blame, Error, Req1} ->
            {error, Blame, Error, store_byte_counts(Req1, BackendClient)}
    end.

store_byte_counts(Req, Client) ->
    {SentNew, RecvNew} = vegur_client:byte_counts(Client),
    {BytesSent, Req2} = cowboy_req:meta(bytes_sent, Req),
    {BytesRecv, Req3} = cowboy_req:meta(bytes_recv, Req2),
    Sent = case {BytesSent, SentNew} of
        {_, undefined} -> BytesSent;
        {undefined, _} -> SentNew;
        {_,_} -> max(BytesSent, SentNew)
    end,
    Recv = case {BytesRecv, RecvNew} of
        {_, undefined} -> BytesRecv;
        {undefined, _} -> RecvNew;
        {_,_} -> max(BytesRecv, RecvNew)
    end,
    Req4 = cowboy_req:set_meta(bytes_sent, Sent, Req3),
    cowboy_req:set_meta(bytes_recv, Recv, Req4).


parse_request(Req) ->
    case check_for_body(Req) of
        {{error, Reason}, _Req} ->
            {error, upstream, Reason};
        {Body, Req1} ->
            parse_request(Body, Req1)
    end.

parse_request(Body, Req) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {Host, Req4} = cowboy_req:host(Req3),
    {Qs, Req5} = cowboy_req:qs(Req4),
    {Headers, Req6} = cowboy_req:keyed_headers(Req5),
    FullPath = case Qs of
        <<>> -> Path;
        _ -> <<Path/binary, "?", Qs/binary>>
    end,
    {Headers2, Req7} = add_proxy_headers(Headers, Req6),
    {{Method, Headers2, Body, FullPath, Host}, Req7}.

add_proxy_headers(Headers, Req) ->
    {Headers1, Req1} = add_request_id(Headers, Req),
    {Headers2, Req2} = add_forwarded(Headers1, Req1),
    {Headers3, Req3} = add_via(Headers2, Req2),
    {Headers4, Req4} = add_connect_time(Headers3, Req3),
    {Headers5, Req5} = add_start_time(Headers4, Req4),
    {Headers6, Req6} = add_total_route_time(Headers5, Req5),
    add_interface_headers(Headers6, Req6).

add_interface_headers(Headers, Req) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    {InterfaceModule, HandlerState, Req2} = vegur_utils:get_interface_module(Req1),
    {InterfaceHeaders, HandlerState1} = InterfaceModule:additional_headers(Log, HandlerState),
    Req3 = vegur_utils:set_handler_state(HandlerState1, Req2),
    InterfaceKeyedHeaders = [{cowboy_bstr:to_lower(Name), Name, Val}
                             || {Name,Val} <- InterfaceHeaders],
    {vegur_utils:add_or_replace_headers(InterfaceKeyedHeaders, Headers), Req3}.

add_start_time(Headers, Req) ->
    {Time, Req1} = vegur_req:start_time(Req),
    HeaderKey = HeaderName = vegur_utils:config(start_time_header),
    {vegur_utils:add_or_replace_header(HeaderKey,
                                       HeaderName,
                                       integer_to_list(timer:now_diff(Time, {0,0,0}) div 1000),
                                       Headers),
     Req1}.

add_connect_time(Headers, Req) ->
    {Time, Req1} = vegur_request_log:get_log_value(connect_time, Req),
    HeaderKey = HeaderName = vegur_utils:config(connect_time_header),
    {vegur_utils:add_or_replace_header(HeaderKey,
                                       HeaderName,
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
    HeaderKey = HeaderName = vegur_utils:config(route_time_header),
    {vegur_utils:add_or_replace_header(HeaderKey, HeaderName,
                                       Time, Headers), Req1}.

add_request_id(Headers, Req) ->
    {RequestId, Req1} = cowboy_req:meta(request_id, Req),
    HeaderKey = HeaderName = vegur_utils:config(request_id_name),
    {vegur_utils:add_or_replace_header(HeaderKey, HeaderName, RequestId, Headers),
     Req1}.

add_forwarded(Headers, Req) ->
    {Headers1, Req2} = case vegur_utils:peer_ip_port(Req) of
                           {{PeerAddress, PeerPort, DestPort}, Req1} ->
                               handle_feature(Req1, {Headers, PeerPort});
                           {{PeerAddress, DestPort}, Req1} ->
                               {Headers, Req1}
                       end,

    {Headers2, Req3} = vegur_utils:add_or_append_header(
        <<"x-forwarded-for">>, <<"X-Forwarded-For">>,
        inet:ntoa(PeerAddress), Headers1, Req2),

    Headers3 =
        case DestPort of
            80 ->
                vegur_utils:add_or_replace_header(
                    <<"x-forwarded-proto">>, <<"X-Forwarded-Proto">>,
                    <<"http">>, Headers2);
            443 ->
                vegur_utils:add_or_replace_header(
                    <<"x-forwarded-proto">>, <<"X-Forwarded-Proto">>,
                    <<"https">>, Headers2);
            _ ->
                Headers2
        end,

    Headers4 = vegur_utils:add_or_replace_header(
        <<"x-forwarded-port">>, <<"X-Forwarded-Port">>,
        integer_to_list(DestPort), Headers3),

    {Headers4, Req3}.

add_via(Headers, Req) ->
    Via = vegur_utils:get_via_value(),
    vegur_utils:add_or_append_header(<<"via">>, <<"Via">>, Via, Headers, Req).

handle_feature(Req, {Headers, PeerPort}) ->
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    case InterfaceModule:feature(peer_port, HandlerState) of
        {enabled, HandlerState2} ->
            Req2 = vegur_utils:set_handler_state(HandlerState2, Req1),
            {vegur_utils:add_or_replace_header(<<"x-forwarded-peer-port">>,
                                               <<"X-Forwarded-Peer-Port">>,
                                              integer_to_list(PeerPort),
                                               Headers), Req2};
        {disabled, HandlerState2} ->
            Req2 = vegur_utils:set_handler_state(HandlerState2, Req1),
            {Headers, Req2}
    end.

check_for_body(Req) ->
    %% We handle the request differently based on whether it's chunked,
    %% has a known length, or if it has no body at all.
    case cowboy_req:has_body(Req) of
        true ->
            case cowboy_req:body_length(Req) of
                {undefined, Req2} ->
                    {{stream, chunked}, Req2};
                {error, badarg} ->
                    {{error, invalid_transfer_encoding}, Req};
                {Length, Req2} ->
                    {{stream, Length}, Req2}
            end;
        false ->
            {<<>>, Req}
    end.
