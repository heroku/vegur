%%% Copyright (c) 2013-2015, Heroku Inc <routing-feedback@heroku.com>.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(vegur_bytepipe_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [pipe_proc, pipe_proc_callback, pipe_proc_timeout,
     pipe_become, pipe_become_callback, pipe_become_timeout].

suite() ->
    [{timetrap, {seconds, 30}}].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Client = proc_lib:start(?MODULE, init, [self(), 8877, [{role,client}]]),
    Server = proc_lib:start(?MODULE, init, [self(), 8888, [{role,server}]]),
    [{client, Client}, {server, Server} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

pipe_proc(Config) ->
    %% Both A and B are processes implementing ranch_transport, allowing
    %% to control them more flexibly than raw TCP ports, of the form
    %% {Transport, Resource}. The data they send and receive is stored in
    %% a log with messages of the form:
    %% {send, Msg}
    %% {recv, Msg}
    %%
    %% This test verifies that using vegur_bytepipe with a process (started
    %% with start_link/2) does pipeline data and closes down everything
    %% when done.
    Client = ?config(client, Config),
    Server = ?config(server, Config),
    {ok, Pipe} = vegur_bytepipe:start_link(Client, Server),
    send(Client, "ping"),
    timer:sleep(100),
    send(Server, "pong"),
    timer:sleep(1000),
    [{send, "ping"},
     {recv, "pong"}] = log(Client),
    [{recv, "ping"},
     {send, "pong"}] = log(Server),
    unlink(Pipe),
    close(Server),
    closed(Server),
    closed(Client),
    false = is_process_alive(Pipe).

pipe_proc_callback(Config) ->
    %% Both A and B are processes implementing ranch_transport, allowing
    %% to control them more flexibly than raw TCP ports, of the form
    %% {Transport, Resource}. The data they send and receive is stored in
    %% a log with messages of the form:
    %% {send, Msg}
    %% {recv, Msg}
    %%
    %% This test verifies that using vegur_bytepipe with a process (started
    %% with start_link/2) does pipeline data and respects the callback procedure
    %% when done.
    Client = ?config(client, Config),
    Server = ?config(server, Config),
    Test = self(),
    Callback = fun(_TransC, PortC, _TransS, _PortS, Event) ->
        case Event of
            {tcp_closed, PortC} -> Test ! {dead, client};
            {tcp_error, PortC, _} -> Test ! {dead, client};
            _ -> Test ! {dead, server}
        end
    end,
    {ok, Pipe} = vegur_bytepipe:start_link(Client, Server, [{on_close, Callback}]),
    send(Client, "ping"),
    timer:sleep(100),
    send(Server, "pong"),
    timer:sleep(1000),
    [{send, "ping"},
     {recv, "pong"}] = log(Client),
    [{recv, "ping"},
     {send, "pong"}] = log(Server),
    unlink(Pipe),
    close(Server),
    closed(Server),
    closed(Client),
    false = is_process_alive(Pipe),
    receive
        {dead, server} -> ok
    after 500 ->
        error(bad_callback)
    end.

pipe_proc_timeout(Config) ->
    %% Both A and B are processes implementing ranch_transport, allowing
    %% to control them more flexibly than raw TCP ports, of the form
    %% {Transport, Resource}.
    %%
    %% This test verifies that using vegur_bytepipe with a process (started
    %% with start_link/2) handles timing out properly based on its callback
    %% and configured timeout value.
    Client = ?config(client, Config),
    Server = ?config(server, Config),
    Parent = self(),
    Callback = fun(TransC, PortC, TransS, PortS, timeout) ->
        TransC:close(PortC),
        TransS:close(PortS),
        Parent ! timeout
    end,
    {ok, Pipe} = vegur_bytepipe:start_link(
        Client,
        Server,
        [{on_timeout, Callback}, {timeout, 500}]
    ),
    unlink(Pipe),
    timer:sleep(1000),
    closed(Server),
    closed(Client),
    false = is_process_alive(Pipe).

pipe_become(Config) ->
    %% Both A and B are processes implementing ranch_transport, allowing
    %% to control them more flexibly than raw TCP ports, of the form
    %% {Transport, Resource}. The data they send and receive is stored in
    %% a log with messages of the form:
    %% {send, Msg}
    %% {recv, Msg}
    %%
    %% This test verifies that using vegur_bytepipe:become/2 allows the
    %% current process to become a pipeline and return with all connections
    %% closed afterwards.
    Client = ?config(client, Config),
    Server = ?config(server, Config),
    %% Here is the tricky bit. Before becoming the pipe we have to
    %% give out orders to send messages and close said pipes at first.
    %% We link it so we crash the test if this doesn't work.
    spawn_link(fun() ->
        timer:sleep(100),
        send(Client, "ping"),
        timer:sleep(100),
        send(Server, "pong"),
        timer:sleep(1000),
        [{send, "ping"},
         {recv, "pong"}] = log(Client),
        [{recv, "ping"},
         {send, "pong"}] = log(Server),
        close(Server)
    end),
    ok = vegur_bytepipe:become(Client, Server),
    closed(Server),
    closed(Client).

pipe_become_callback(Config) ->
    %% Both A and B are processes implementing ranch_transport, allowing
    %% to control them more flexibly than raw TCP ports, of the form
    %% {Transport, Resource}.
    %%
    %% This test verifies that using vegur_bytepipe:become/3 allows the
    %% current process to become a pipeline and that it handles the proper
    %% configured timeouts and callbacks afterwards.
    Client = ?config(client, Config),
    Server = ?config(server, Config),
    Callback = fun(TransC, PortC, TransS, PortS, timeout) ->
        TransC:close(PortC),
        TransS:close(PortS),
        timeout
    end,
    timeout = vegur_bytepipe:become(
        Client,
        Server,
        [{on_timeout, Callback}, {timeout, 200}]),
    closed(Server),
    closed(Client).

pipe_become_timeout(Config) ->
    %% Both A and B are processes implementing ranch_transport, allowing
    %% to control them more flexibly than raw TCP ports, of the form
    %% {Transport, Resource}. The data they send and receive is stored in
    %% a log with messages of the form:
    %% {send, Msg}
    %% {recv, Msg}
    %%
    %% This test verifies that using vegur_bytepipe:become/3 allows the
    %% current process to become a pipeline and return based on its callback
    %% afterwards.
    Client = ?config(client, Config),
    Server = ?config(server, Config),
    Callback = fun(TransC, PortC, TransS, PortS, Event) ->
        case Event of
            {tcp_closed, PortC} -> {closed, {TransC,PortC}};
            {tcp_error, PortC, _} -> {error, {TransC,PortC}};
            {tcp_closed, PortS} -> {closed, {TransS,PortS}};
            {tcp_error, PortS, _} -> {error, {TransS,PortS}}
        end
    end,
    %% Here is the tricky bit. Before becoming the pipe we have to
    %% give out orders to send messages and close said pipes at first.
    %% We link it so we crash the test if this doesn't work.
    spawn_link(fun() ->
        timer:sleep(100),
        send(Client, "ping"),
        timer:sleep(100),
        send(Server, "pong"),
        timer:sleep(1000),
        [{send, "ping"},
         {recv, "pong"}] = log(Client),
        [{recv, "ping"},
         {send, "pong"}] = log(Server),
        close(Server)
    end),
    {closed, Server} = vegur_bytepipe:become(Client, Server, [{on_close, Callback}]),
    closed(Server),
    %% and here the client should still be alive
    ok = setopts(Client, [{active, false}]), % if this works, the socket is open
    close(Client),
    closed(Client).


%%%===================================================================
%%% Helpers: Ranch Transport Behaviour
%%%===================================================================

%% This is a send from this module, so we forward it to the process in
%% charge, and it can be sent on behalf of the server or the client.
send({?MODULE,Conn}, IoData) ->
    global:send({n,l,Conn}, {send, {gen_tcp,Conn}, IoData});
%% This is a send call from the bytepipe, and represents the proxy making
%% the call
send(Port, IoData) ->
    cthr:pal("proxy send (through ~p): ~p", [Port, IoData]),
    gen_tcp:send(Port, IoData).

recv({?MODULE,Port}, Int, Timeout) -> % support in-suite calls
    recv(Port, Int, Timeout);
recv(Port, Int, Timeout) ->
    cthr:pal("passive recv (through ~p)", [Port]),
    gen_tcp:recv(Port, Int, Timeout).

setopts({?MODULE, Port}, Opts) -> % support in-suite calls
    setopts(Port, Opts);
setopts(Port, Opts) ->
    inet:setopts(Port, Opts).

close({?MODULE,Port}) -> % support in-suite calls
    case global:whereis_name({n,l,Port}) of
        undefined -> ok;
        Pid ->
            unlink(Pid), % no error on shutdown please
            Pid ! {close,{?MODULE,Port}}
    end;
close(Port) ->
    gen_tcp:close(Port).

controlling_process({?MODULE,Port}, Pid) -> % support in-suite calls
    controlling_process(Port, Pid);
controlling_process(Port, Pid) ->
    gen_tcp:controlling_process(Port, Pid).

match_port({?MODULE, Port}) -> % support in-suite calls
    Port;
match_port(Port) ->
    Port.

messages() -> {tcp, tcp_closed, tcp_error}.

%%%===================================================================
%%% Helpers: Socket manipulation and middlemen
%%%===================================================================

%% Interface to the middlemen processes
closed({?MODULE,Port}) ->
    case global:whereis_name({n,l,Port}) of
        undefined -> ok;
        Pid ->
            Ref = erlang:monitor(process, Pid),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            after 5000 ->
                error(not_dead)
            end
    end.

log({?MODULE, Conn}) ->
    Ref = make_ref(),
    global:send({n,l,Conn}, {log, self(), Ref}),
    receive
        {Ref, Log} -> Log
    after 5000 ->
        error(log_timeout)
    end.

%% The middlemen processes
init(Parent, Port, Opts) ->
    erlang:monitor(process, Parent),
    S = self(),
    spawn_link(fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [{reuseaddr, true}]),
        {ok, Accept} = gen_tcp:accept(Listen),
        ok = gen_tcp:controlling_process(Accept, S),
        S ! {accept, Accept}
    end),
    spawn_link(fun() ->
        {ok, Conn} = gen_tcp:connect({127,0,0,1}, Port, []),
        ok = gen_tcp:controlling_process(Conn, S),
        S ! {conn, Conn}
    end),
    Accept = receive {accept, Acc} -> Acc end,
    Conn = receive {conn, Con} -> Con end,
    cthr:pal("~p Acc:~p Conn~p",[Opts, Accept, Conn]),
    %% We keep in state the port defined by our role: 'client'
    %% keeps the Conn port (client-side) and exposes Accept (server-side).
    %% 'server' keeps the Accept port (endpoint-side) and exposes Conn
    %% (server-side).
    %%
    %% The ports are also set to active false to let the proxy change
    %% whatever it needs without getting unexpected data.
    case proplists:get_value(role, Opts) of
        client ->
            global:register_name({n,l,Accept}, self()),
            inet:setopts(Accept, [{active,false}]),
            gen_tcp:controlling_process(Accept, Parent),
            proc_lib:init_ack(Parent, {?MODULE,Accept}),
            loop(Conn, []);
        server ->
            global:register_name({n,l,Conn}, self()),
            inet:setopts(Conn, [{active,false}]),
            gen_tcp:controlling_process(Conn, Parent),
            proc_lib:init_ack(Parent, {?MODULE,Conn}),
            loop(Accept, [])
    end.

loop(Port, Log) ->
    inet:setopts(Port, [{active, once}]),
    receive
        %% API stuff
        {send, {Transport, _Sock}, Msg} ->
            cthr:pal("module send (~p -> ~p): ~p", [Port, _Sock, Msg]),
            Transport:send(Port, Msg),
            loop(Port, [{send, Msg} | Log]);
        {close, {Transport, _Sock}} ->
            Transport:close(Port);
        {log, From, Ref} ->
            From ! {Ref, lists:reverse(Log)},
            loop(Port, Log);
        %%  TCP handling
        {tcp, Port, Msg} ->
            cthr:pal("active recv (through ~p): ~p", [Port,Msg]),
            loop(Port, [{recv, Msg} | Log]);
        {tcp_closed, Port} ->
            exit(tcp_closed);
        {tcp_error, Port, Reason} ->
            exit({tcp_error, Reason});
        {'DOWN', _Ref, process, _Parent, _Reason} ->
            exit(parent_down);
        OTHER ->
            cthr:pal("???: ~p", [OTHER]),
            exit('WHAT THE HELL')
    end.
