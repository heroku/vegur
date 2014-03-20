-module(vegur_proxy_middleware).

-behaviour(cowboy_middleware).
-export([execute/2]).

-record(state, { backend_client :: term()
                 ,env
               }).

execute(Req, Env) ->
    {Log, Req1} = cowboy_req:meta(logging, Req),
    Log1 = vegur_req_log:stamp(pre_proxy, Log),
    {Client, Req2} = cowboy_req:meta(backend_connection, Req1),
    case vegur_req_log:log(service_time,
                           fun() ->
                                   proxy(Req2, #state{backend_client = Client, env = Env})
                           end, Log1) of
        {{ok, Req3, #state{backend_client=Client1}}, Log2} ->
            Req4 = store_byte_counts(Req3, Client1),
            Req5 = cowboy_req:set_meta(status, successful, Req4),
            Req6 = cowboy_req:set_meta(logging, Log2, Req5),
            {halt, Req6};
        {{error, Blame, Reason, Req3}, Log2} ->
            {HttpCode, Req4} = vegur_utils:handle_error({Blame, Reason}, Req3),
            Req5 = store_byte_counts(Req4, Client),
            Req6 = cowboy_req:set_meta(logging, Log2, Req5),
            {error, HttpCode, Req6}
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
            {error, Blame, Error, store_byte_counts(Req, BackendClient)}
    end.

read_backend_response(Req, #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:read_backend_response(Req, BackendClient) of
        {ok, Code, RespHeaders, Req1, BackendClient1} ->
            handle_backend_response(Code, RespHeaders, Req1,
                                    State#state{backend_client=BackendClient1});
        {error, Blame, Error} ->
            {error, Blame, Error, store_byte_counts(Req, BackendClient)}
    end.

handle_backend_response(Code, RespHeaders, Req, State) ->
    Req1 = cowboy_req:set_meta(response_code, Code, Req),
    {Type, Req2} = cowboy_req:meta(request_type, Req1, []),
    case lists:sort(Type) of
        [] ->
            http_request(Code, RespHeaders, Req2, State);
        [upgrade] ->
            upgrade_request(Code, RespHeaders, Req2, State)
    end.

upgrade_request(101, Headers, Req, #state{backend_client=BackendClient}=State) ->
    {Result, Req1, BackendClient1} = vegur_proxy:upgrade(Headers, Req, BackendClient),
    case Result of
        timeout ->
            {error, undefined, timeout, store_byte_counts(Req, BackendClient1)};
        done ->
            {ok, Req1, State#state{backend_client=BackendClient1}}
    end;
upgrade_request(Code, Headers, Req, State) ->
    http_request(Code, Headers, Req, State).

http_request(Code, Headers, Req,
             #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:relay(Code, Headers, Req, BackendClient) of
        {ok, Req1, BackendClient1} ->
            {ok, Req1, State#state{backend_client=BackendClient1}};
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
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {Host, Req4} = cowboy_req:host(Req3),
    {Headers, Req5} = cowboy_req:headers(Req4),
    %% We handle the request differently based on whether it's chunked,
    %% has a known length, or if it has no body at all.
    {Body, Req7} = 
        case cowboy_req:has_body(Req5) of
            true ->
                case cowboy_req:body_length(Req5) of
                    {undefined, Req6} ->
                        {{stream, chunked}, Req6};
                    {Length, Req6} ->
                        {{stream, Length}, Req6}
                end;
            false ->
                {<<>>, Req5}
        end,
    {Headers2, Req8} = add_proxy_headers(Headers, Req7),
    {{Method, Headers2, Body, Path, Host}, Req8}.

add_proxy_headers(Headers, Req) ->
    {Headers1, Req1} = add_request_id(Headers, Req),
    {Headers2, Req2} = add_forwarded(Headers1, Req1),
    {Headers3, Req3} = add_via(Headers2, Req2),
    {Headers4, Req4} = add_connect_time(Headers3, Req3),
    add_total_route_time(Headers4, Req4).

add_connect_time(Headers, Req) ->
    {Time, Req1} = vegur_request_log:get_log_value(connect_time, Req),
    {vegur_utils:add_or_replace_header(vegur_utils:config(connect_time_header),
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
    {vegur_utils:add_or_replace_header(vegur_utils:config(route_time_header),
                                       Time, Headers), Req1}.

add_request_id(Headers, Req) ->
    {RequestId, Req1} = cowboy_req:meta(request_id, Req),
    {vegur_utils:add_or_replace_header(vegur_utils:config(request_id_name), RequestId, Headers),
     Req1}.

add_forwarded(Headers, Req) ->
    {{PeerAddress, DestPort}, Req1} = vegur_utils:peer_ip_port(Req),
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
    vegur_utils:config(instance_name, <<"vegur">>).
