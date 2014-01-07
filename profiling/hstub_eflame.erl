#!/usr/bin/env escript
%%! -pa ebin deps/*/ebin
-module(hstub_eflame).
-mode(compile).
-export([main/1, run/0, init_cowboy_req/1]).

main(_) ->
    run(),
    halt(0).

run() ->
    %% Start backend listener
    {ok, Listen} = gen_tcp:listen(0, [{active, false},binary]),
    {ok, ServerPort} = inet:port(Listen),
    %% Start proxy listener
    {ok, ListenProxy} = gen_tcp:listen(0, [{active, false},binary]),
    %% Load & Mock
    application:ensure_all_started(meck),
    application:load(hstub),
    ok = application:set_env(hstub, interface_module, hstub_interface),
    meck:new(hstub_stub, [passthrough, no_link]),
    meck:expect(hstub_stub, service_backend, fun(_) -> {{127,0,0,1}, ServerPort} end),
    %% Start e-flame and prepare for benchmarks
    {ok, _} = application:ensure_all_started(eflame),
    application:ensure_all_started(hstub),
    preload_modules(),
    %% Gun it!
    Res = trace(ListenProxy, Listen),
    io:format("Res: ~p~n",[Res]),
    %% Generate SVG
    io:format("~s",[os:cmd("deps/eflame/stack_to_flame.sh < stacks.out > flame.svg")]),
    ok.

preload_modules() ->
    Mods = [cowboy_clock,hstub_proxy_middleware, crypto, os, queue,
            hstub_lookup_service_middleware,uuid, ranch_app,
            hstub_midjan_middleware, hstub_upgrade_middleware,hstub_client,
            hstub_midjan_translator, ranch_listener_sup,ranch,hstub_service,
            ranch_acceptor, midjan_core,hstub_utils, erequest_id, cowboy,
            ranch_proxy,hstub_domains, hstub_request_log,hstub_req_log,
            hstub_validate_headers, cowboy_protocol,hstub_proxy,
            hstub_healthcheck_middleware,hstub_continue_middleware,
            hstub_lookup,hstub_maintenance_middleware,
            ranch_server,cowboy_bstr,ranch_tcp, cowboy_http,
            hstub_lookup_domain_middleware, cowboy_req],
    [M:module_info() || M <- Mods],
    ok.

trace(ListenProxy, ListenPort) ->
    %% Start the basic request / sending it
    {ClientPort, ProxyPort} = start_req(ListenProxy, ListenPort),
    %% Be ready to handle it once the proxy deals with it
    spawn_link(fun() -> handle_response(ClientPort, ListenPort) end),
    %% Let the proxy handle it and measure!
    eflame:apply(normal_with_children, "stacks.out",
                 ?MODULE, init_cowboy_req, [ProxyPort]).

start_req(ListenProxy, Listen) ->
    %% Build the request
    Req = text_req(Listen),
    %% Accept proxy-side
    Ref = make_ref(),
    start_acceptor(Ref, ListenProxy),
    %% Connect client-side to the proxy
    PortNum = element(2,inet:port(ListenProxy)),
    {ok, Client} = gen_tcp:connect({127,0,0,1}, PortNum, [{active,false}, binary],1000),
    %% Send the request and fetch back the proxy-side socket
    ok = gen_tcp:send(Client, Req),
    Proxy = get_accepted(Ref),
    {Client, Proxy}.

handle_response(Client, Listen) ->
    %% The client has sent its data already, we can mostly ignore it
    %% unless we plan on reading req content or making sure it made
    %% it through.
    %% Build the response
    Response = text_response(),
    %% Accept backend-side and send the response
    {ok, Server} = gen_tcp:accept(Listen),
    gen_tcp:send(Server, Response),
    %% Confirm that the client received the response and shut down
    io:format("CLI RECV: ~p~n", [gen_tcp:recv(Client, 0)]),
    gen_tcp:close(Client),
    gen_tcp:close(Server).

start_acceptor(Ref, Listen) ->
    %% The acceptor process will just take the connection and shoot
    %% it back to us. It's a side-process because the connection can
    %% only be established once a client calls, and the original
    %% process (`Parent') will be the one to act as the client.
    Parent = self(),
    spawn_link(fun() ->
        io:format("accept on ~p~n",[element(2,inet:port(Listen))]),
        {ok, Accept} = gen_tcp:accept(Listen, 30000),
        ok = gen_tcp:controlling_process(Accept, Parent),
        io:format("accepted ~p~n",[element(2,inet:port(Listen))]),
        Parent ! {Ref, Accept}
    end),
    ok.

get_accepted(Ref) ->
    %% Fetch the accepted connection from start_acceptor/2
    receive
        {Ref, Accepted} -> Accepted
    after 5000 ->
        error(too_long)
    end.


init_cowboy_req(ProxyPort) ->
    %% ranch has an ack mechanism that we must use here
    Ref = make_ref(),
    self() ! {shoot, Ref},
    %% This falls into a wait loop, so this is the call we should
    %% profile. The rest will come from there. Closing the connection
    %% after a response is received by the client will cut things short
    %% and return here.
    Transport = ranch_tcp,
    cowboy_protocol:init(Ref, ProxyPort, Transport, cowboy_opts()).

cowboy_opts() ->
    [{env, [{interface_module, hstub_stub}]}
    ,{middlewares, [hstub_midjan_middleware]}
    ,{onrequest, fun hstub_request_log:new/1}].

text_req(Listen) ->
    PortStr = integer_to_list(element(2,inet:port(Listen))),
    "POST /path HTTP/1.1\r\n"
    "Host: 127.0.0.1:"++PortStr++"\r\n"
    "Content-Length: 1000\r\n"
    "Content-Type: text/plain\r\n"
    "User-Agent: Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5 (.NET CLR 3.5.30729)\r\n"
    "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
    "Accept-Language: en-us,en;q=0.5\r\n"
    "Accept-Encoding: gzip,deflate\r\n"
    "Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7\r\n"
    "Cookie: PHPSESSID=r2t5uvjq435r4q7ib3vtdjq120\r\n"
    "Pragma: no-cache\r\n"
    "Cache-Control: no-cache\r\n"
    "\r\n"
    ++ lists:duplicate(1000, $a).

text_response() ->
    "HTTP/1.1 200 OK\r\n"
    "Date: Fri, 31 Dec 1999 23:59:59 GMT\r\n"
    "Content-Type: text/plain\r\n"
    "Content-Length: 1000\r\n"
    "X-Powered-By: Writing stuff by hand/0.8\r\n"
    "Pragma: public\r\n"
    "Expires: Sat, 28 Nov 2009 05:36:25 GMT\r\n"
    "Etag: \"pub1259380237;gz\"\r\n"
    "Cache-Control: max-age=3600, public\r\n"
    "Content-Type: text/html; charset=UTF-8\r\n"
    "Last-Modified: Sat, 28 Nov 2009 03:50:37 GMT\r\n"
    "X-Pingback: http://example.org/xmlrpc.php\r\n"
    "Content-Encoding: gzip\r\n"
    "Vary: Accept-Encoding, Cookie, User-Agent\r\n"
    "\r\n"
    ++ lists:duplicate(1000, $a).
