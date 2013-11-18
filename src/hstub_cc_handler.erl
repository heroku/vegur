%% @copyright Heroku (2013)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Cowboy Client HTTP handler for hstub.
%% @end
-module(hstub_cc_handler).

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
    {ok, Client} = hstub_client:init([]),
    case hstub_client:connect(ranch_tcp,
                              ip_to_tuple(IP),
                              Port,
                              100,
                              Client) of
        {ok, Client2} ->
            {connected, Req, State#state{backend_client=Client2}};
        {error, _} = Err ->
            {Err, Req, State}
    end.

proxy(Req, State) ->
    {BackendReq, Req2} = parse_request(Req),
    case send_request(BackendReq, State) of
        {ok, Status, RespHeaders, State2} ->
            case body(State2) of
                {ok, Body, State3} ->
                    respond(Status, response_headers(RespHeaders),
                            Body, Req2,
                            State3);
                {error, _} = Err ->
                    respond_err(Err, Req, backend_close(State))
            end;
        {error, _} = Err ->
            respond_err(Err, Req, backend_close(State))
    end.

send_request({Method, Headers, Body, URL, Path},
             State = #state{backend_client=Client}) ->
    Request = hstub_client:request_to_iolist(Method,
                                             request_headers(Headers),
                                             Body,
                                             'HTTP/1.1',
                                             URL,
                                             Path),
    case hstub_client:raw_request(Request, Client) of
        {ok, Client2} ->
            case hstub_client:response(Client2) of
                {error, _} = Err -> Err;
                {ok, Status, RespHeaders, Client3} ->
                    {ok, Status, RespHeaders,
                     State#state{backend_client=Client3}}
            end;
        {error, _Err} = Err -> Err
    end.

backend_close(State = #state{backend_client = undefined}) -> State;
backend_close(State = #state{backend_client = Client}) ->
    hstub_client:close(Client),
    State#state{backend_client = undefined}.

body(State = #state{backend_client = Client}) ->
    case hstub_client:response_body(Client) of
        {error, _Err} = Err -> Err;
        {ok, Body, Client2} ->
            {ok, Body, State#state{backend_client = Client2}}
    end.

parse_request(Req) ->
    {Method, Req2} =  cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    {Host, Req4} = cowboy_req:host(Req3),
    {Port, Req5} = cowboy_req:port(Req4),
    {Headers, Req6} = cowboy_req:headers(Req5),
    {ok, Body, Req7} = cowboy_req:body(Req6),
    {{Method,
      Headers,
      Body,
      iolist_to_binary([Host, ":", integer_to_list(Port)]),
      Path},
     Req7}.

ip_to_tuple(IP) when is_binary(IP) ->
    IPs = binary_to_list(IP),
    case inet:parse_address(IPs) of
        {ok, Parsed} ->
            Parsed;
        {error, einval} ->
            IPs
    end.

%% Strip Connection header on response.
response_headers(Headers0) ->
    lists:foldl(fun (F, H) ->
                        F(H)
                end,
                Headers0,
                [fun delete_keepalive_header/1
                ]).
%% Strip Connection header on request.
request_headers(Headers0) ->
    lists:foldl(fun (F, H) ->
                        F(H)
                end,
                Headers0,
                [fun delete_keepalive_header/1
                ,fun delete_host_header/1
                ,fun add_connection_close/1
                ]).


delete_keepalive_header(Hdrs) ->
    lists:delete({<<"connection">>, <<"keepalive">>}, Hdrs).

delete_host_header(Hdrs) ->
    lists:keydelete(<<"host">>, 1, Hdrs).

add_connection_close(Hdrs) ->
    case lists:keymember(<<"connection">>, 1, Hdrs) of
        true -> Hdrs;
        false -> [{<<"connection">>, <<"close">>} | Hdrs]
    end.
