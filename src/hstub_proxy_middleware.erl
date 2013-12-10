-module(hstub_proxy_middleware).
-export([execute/2]).

-record(state, { backend_client :: term()
                 ,env
               }).

execute(Req, Env) ->
    {Service, Req1} = cowboy_req:meta(service, Req, undefined),
    connect(Service, Req1, Env).

connect(Service, Req, Env) ->
    case hstub_proxy:backend_connection(Service) of
        {connected, Client} ->
            proxy(Req, #state{backend_client = Client,
                              env = Env});
        {error, _Reason} ->
            {error, 503, Req}
    end.

proxy(Req, State) ->
    {BackendReq, Req1} = parse_request(Req),
    send_to_backend(BackendReq, Req1, State).

send_to_backend({Method, Header, {stream, _}=Body, Path, Url}, Req,
                #state{backend_client=BackendClient}=State) ->
    case hstub_proxy:send_request(Method, Header, Body, Path, Url, Req, BackendClient) of
        {done, Req1, BackendClient1} ->
            read_backend_response(Req1, State#state{backend_client=BackendClient1});
        {error, _Error} ->
            %% @todo handle correctly
            {error, 503, Req}
    end;
send_to_backend({Method, Header, Body, Path, Url}, Req, 
                #state{backend_client=BackendClient}=State) ->
    case hstub_proxy:send_request(Method, Header, Body, Path, Url, Req, BackendClient) of
        {done, Req1, BackendClient1} ->
            read_backend_response(Req1, State#state{backend_client=BackendClient1});
        {error, _Error} ->
            %% @todo handle correctly
            {error, 503, Req}
    end.

read_backend_response(Req, #state{backend_client=BackendClient}=State) ->
    case hstub_proxy:read_response(BackendClient) of
        {ok, Code, RespHeaders, BackendClient1} ->
            case cowboy_req:meta(upgrade_requested, Req, false) of
                {true, Req1} ->
                    upgrade_request(Code, RespHeaders, Req1,
                                    State#state{backend_client=BackendClient1});
                {_, Req1} ->
                    http_request(Code, RespHeaders, Req1,
                                 State#state{backend_client=BackendClient1})
            end;
        {error, _Error} ->
            %% @todo close backend
            {error, 503, Req}
    end.

upgrade_request(101, Headers, Req, #state{backend_client=BackendClient,
                                          env=Env}) ->
    {done, Req1} = hstub_proxy:upgrade(Headers, Req, BackendClient),
    {ok, Req1, Env};
upgrade_request(Code, Headers, Req, State) ->
    http_request(Code, Headers, Req, State).

http_request(Code, Headers, Req, #state{backend_client=BackendClient, env=Env}) ->
    case hstub_proxy:relay(Code, Headers, Req, BackendClient) of
        {ok, Req1, _Client1} ->
            {ok, Req1, Env};
        {error, _Error, Req1} ->
            {error, 503, Req1}
    end.

parse_request(Req) ->
    {Method, Req2} =  cowboy_req:method(Req),
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
    {Headers3, Req3}.

add_request_id(Headers, Req) ->
    {RequestId, Req1} = cowboy_req:meta(request_id, Req),
    RequestId1 = [integer_to_list(B, 16) || <<B>> <= RequestId],
    hstub_utils:add_if_missing_header(<<"request-id">>, RequestId1, Headers, Req1).

add_forwarded(Headers, Req) ->
    Transport = cowboy_req:get(transport, Req),
    {{PeerAddress, PeerPort}, Req1} =
        case Transport:name() of
            proxy_protocol_tcp ->
                ProxySocket = cowboy_req:get(socket, Req),
                {ok, {PeerInfo, _}} = Transport:proxyname(ProxySocket),
                {PeerInfo, Req};
            _ ->
                cowboy_req:peer(Req)
        end,
    {Headers1, Req2} = hstub_utils:add_or_append_header(<<"x-forwarded-for">>, inet_parse:ntoa(PeerAddress),
                                                        Headers, Req1),
    Headers2 =
        case PeerPort of
            80 ->
                Headers1 ++ [{<<"x-forwarded-proto">>, <<"http">>}];
            443 ->
                Headers1 ++ [{<<"x-forwarded-proto">>, <<"https">>}];
            _ ->
                Headers1
        end,
    {Headers2, Req2}.

add_via(Headers, Req) ->
    hstub_utils:add_or_append_header(<<"via">>, <<"hstub">>, Headers, Req).
