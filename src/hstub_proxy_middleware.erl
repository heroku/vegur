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

http_request(Code, Headers, Req, #state{backend_client=BackendClient,
                                        env=Env}) ->
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
    {{Method, Headers, Body, Path, Url}, Req7}.
