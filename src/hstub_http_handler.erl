%% @copyright Heroku (2013)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Default HTTP handler for hstub.
%% @end
-module(hstub_http_handler).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

-include("include/hstub_log.hrl").

-record(state,
        {backend_addr,
         backend_client}).

init({_Any, http}, Req, []) ->
    {ok, Req, #state{}}.

handle(Req, State) ->
    {Req2, State2} = route(Req, State),
    case connect(Req2, State2) of
        {connected, Req3, State3} ->
            proxy(Req3, State3);
        {Err, Req3, State3} ->
            respond_err(Err, Req3, State3)
    end.

respond_err(Error, Req, State) ->
    respond(503,
            [{<<"Content-Type">>, <<"text/plain">>}],
            io_lib:format("~p", [Error]),
            Req, State).

respond(Code, Headers, Body, Req, State) ->
    {ok, Req2} = cowboy_req:reply(Code,
                                  Headers,
                                  Body,
                                  Req),
    {ok, Req2, State}.


terminate(_Reason, _Req, _State) ->
    ok.

route(Req, State) ->
    case cowboy_req:host(Req) of
        {_Host, Req2} ->
            {Req2,
             State#state{backend_addr=hstub_app:config(backend)}}
    end.

connect(Req, State = #state{backend_addr={IP, Port}}) ->
    case hackney:connect(hackney_tcp_transport,
                         ip_to_tuple(IP),
                         Port,
                         [{connect_timeout, 100},
                          {recv_timeout, 5000}]) of
        {ok, Client} ->
            {connected, Req, State#state{backend_client=Client}};
        {error, _} = Err ->
            {Err, Req, State}
    end.

proxy(Req, State = #state{backend_client=Client}) ->
    {BackendReq, Req2} = parse_request(Req),
    case hackney:send_request(Client, BackendReq) of
        {ok, Status, RespHeaders, Client2} ->
            case hackney:body(Client) of
                {ok, Body, Client2} ->
                    respond(Status, response_headers(RespHeaders),
                            Body, Req2,
                            State#state{backend_client=Client2});
                {error, _} = Err ->
                    respond_err(Err, Req, backend_close(State))
            end;
        {error, _} = Err ->
            respond_err(Err, Req, backend_close(State))
    end.

backend_close(State = #state{backend_client = undefined}) -> State;
backend_close(State = #state{backend_client = Client}) ->
    hackney:close(Client),
    State#state{backend_client = undefined}.

parse_request(Req) ->
    {Method, Req2} =  cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {Headers, Req4} = cowboy_req:headers(Req3),
    {ok, Body, Req5} = cowboy_req:body(Req4),
    {{Method,
      Path,
      Headers,
      Body},
     Req5}.

ip_to_tuple(IP) when is_binary(IP) ->
    IPs = binary_to_list(IP),
    case string:tokens(IPs, ".") of
        [A,B,C,D] ->
            {list_to_integer(A),
             list_to_integer(B),
             list_to_integer(C),
             list_to_integer(D)};
        _ ->
            IPs
    end.

%% Strip Connection header on response.
response_headers(Headers0) ->
    lists:keydelete(<<"connection">>, 1, Headers0).
