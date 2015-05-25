%%% This module implements a bi-directional pipeline
%%% to take two different ports and get them to communicate
%%% together.
%%%
%%% Its interface is based on Ranch's transports (which can
%%% be used by adding -behaviour(ranch_transport). to the module),
%%% but more specifically requires:
%%%
%%% - messages/0
%%% - recv/3
%%% - send/2
%%% - setopts/2
%%% - close/1
%%% - controlling_process/2
%%% ? peername/1
%%% ? sockname/1
-module(vegur_bytepipe).

%% API
-export([start_link/2, start_link/3, become/2, become/3]).
%% Callbacks provided
-export([cb_close/5, cb_exit/5]).
%% internal exports
-export([init/4]).


-record(state, {client_transport :: module(),
                client_port :: any(),
                client_match :: port(),
                client_msg :: {OK::atom(), Closed::atom(), Error::atom()},
                server_transport :: module(),
                server_port :: any(),
                server_match :: port(),
                server_msg :: {OK::atom(), Closed::atom(), Error::atom()},
                timeout=infinity :: infinity | non_neg_integer(),
                on_close :: fun(),
                on_timeout :: fun()}).

%% @doc Equivalent to `start_link(Client, Server, [])'.
-spec start_link(Client, Server) -> {ok, pid()} when
    Client :: Transport,
    Server :: Transport,
    Transport :: {module(), any()}.
start_link(Client, Server) ->
    start_link(Client, Server, []).

%% @doc Start a pipe process to handle the load in a third party instead
%% of within the process' current flow. Note that by default the process never
%% gives back the connections and keeps working them until either closes, at
%% which point it also closes them on its own.
%%
%% The process can instead be configured with a callback, which will be called
%% on either port erroring out or being closed, by passing a fun of the form
%% `fun(TransportClient, PortClient, TransportServer, PortServer, Event)' and
%% returns any term at all. The `Event' is of the form `{ClosedAtom, Port}' or
%% `{ErrorAtom, Port, Reason}', where `ClosedAtom' and `ErrorAtom' match the
%% format defined in the ranch transport behaviour.
-spec start_link(Client, Server, Opts) -> {ok, pid()} when
    Client :: Transport,
    Server :: Transport,
    Transport :: {module(), any()},
    Opts :: [{on_close, fun()}|
             {on_timeout, fun()}|
             {timeout, non_neg_integer()}].
start_link(Client={TransC,ConnC}, Server={TransS,ConnS}, Opts) ->
    {ok, Pid, Ref} = proc_lib:start_link(?MODULE, init, [self(), Client, Server, Opts]),
    %% hand-off the ports
    ok = TransC:controlling_process(ConnC, Pid),
    ok = TransS:controlling_process(ConnS, Pid),
    %% tell the worker about it
    Pid ! Ref,
    {ok, Pid}.

%% @doc Equivalent to `become(Client, Server, [])'.
-spec become(Client, Server) -> CallbackReturn::any() when
    Client :: Transport,
    Server :: Transport,
    Transport :: {module(), any()}.
become(Client, Server) ->
    become(Client, Server, []).

%% @doc Upon calling this function, a process enters a receive loop to handle
%% events to be proxied over the byte pipe. By default, the function will close
%% both connections when either of them terminates and return `ok'.
%%
%% This behaviour can instead be configured with a callback, which will be
%% called on either port erroring out or being closed, by passing a fun of the
%% form `fun(TransportClient, PortClient, TransportServer, PortServer, Event)'
%% and returns any term at all. The `Event' is of the form `{ClosedAtom, Port}'
%% or `{ErrorAtom, Port, Reason}', where `ClosedAtom' and `ErrorAtom' match the
%% format defined in the ranch transport behaviour.
%%
%% The term returned by the callback is the one to be returned by the function.
-spec become(Client, Server, Opts) -> CallbackReturn::any() when
    Client :: Transport,
    Server :: Transport,
    Transport :: {module(), any()},
    Opts :: [{on_close, fun()}|
             {on_timeout, fun()}|
             {timeout, non_neg_integer()}].
become({TransC, PortC}, {TransS, PortS}, Opts) ->
    State = #state{client_transport=TransC,
                   client_port=PortC,
                   client_match=port(TransC, PortC),
                   client_msg=TransC:messages(),
                   server_transport=TransS,
                   server_port=PortS,
                   server_match=port(TransS, PortS),
                   server_msg=TransS:messages(),
                   timeout=proplists:get_value(timeout, Opts, infinity),
                   on_close=proplists:get_value(on_close, Opts, fun cb_close/5),
                   on_timeout=proplists:get_value(on_timeout, Opts, fun cb_close/5)},
    ok = TransC:setopts(PortC, [{active, once}]),
    ok = TransS:setopts(PortS, [{active, once}]),
    loop(State).


%% @private initialization of the worker proces. We use raw proc_lib stuff
%% because we: a) need a synchronous start, b) do not really want the
%% overhead of OTP for a tight loop like this.
init(Parent, {TransC, PortC}, {TransS, PortS}, Opts) ->
    State = #state{client_transport=TransC,
                   client_port=PortC,
                   client_match=port(TransC, PortC),
                   client_msg=TransC:messages(),
                   server_transport=TransS,
                   server_port=PortS,
                   server_match=port(TransS, PortS),
                   server_msg=TransS:messages(),
                   timeout=proplists:get_value(timeout, Opts, infinity),
                   on_close=proplists:get_value(on_close, Opts, fun cb_exit/5),
                   on_timeout=proplists:get_value(on_timeout, Opts, fun cb_exit/5)},
    Ref = make_ref(),
    proc_lib:init_ack(Parent, {ok, self(), Ref}),
    receive
        Ref -> % we got control over the ports, test it.
            ok = TransC:setopts(PortC, [{active, once}]),
            ok = TransS:setopts(PortS, [{active, once}]),
            loop(State)
    after 1000 ->
        exit(start_timeout)
    end.

%% @doc When one of the connection is gone, this callback closes
%% the the remaining connection and returns 'ok'
%%
%% It's the default handler used by become/2.
cb_close(TransC, PortC, TransS, PortS, Event) ->
    case Event of
        {_, PortS} -> TransC:close(PortC);
        {_, PortC} -> TransS:close(PortS);
        {_, PortS, _Reason} -> TransC:close(PortC);
        {_, PortC, _Reason} -> TransS:close(PortS);
        timeout ->
            TransC:close(PortC),
            TransS:close(PortS)
    end,
    ok.

%% @doc When one of the connection is gone, this callback closes
%% the the remaining connection and exits the process.
%%
%% It's the default handler used by start_link/2.
cb_exit(TransC, PortC, TransS, PortS, Event) ->
    cb_close(TransC, PortC, TransS, PortS, Event),
    exit(Event).

%% @private core loop that does transportation based on
%% whatever each port receives.
loop(S=#state{client_transport=TransC, client_port=PortC,
              client_match=MatchC, client_msg={OKC, ClosedC, ErrC},
              server_transport=TransS, server_port=PortS,
              server_match=MatchS, server_msg={OKS, ClosedS, ErrS},
              timeout=Timeout,
              on_close=CloseCallback,
              on_timeout=TimeoutCallback}) ->
    receive
        {OKS, MatchS, Data} ->
            TransC:send(PortC, Data),
            TransS:setopts(PortS, [{active,once}]),
            loop(S);
        {OKC, MatchC, Data} ->
            TransS:send(PortS, Data),
            TransC:setopts(PortC, [{active,once}]),
            loop(S);
        {ClosedS, MatchS} ->
            CloseCallback(TransC, PortC, TransS, PortS, {ClosedS, PortS});
        {ClosedC, MatchC} ->
            CloseCallback(TransC, PortC, TransS, PortS, {ClosedC, PortC});
        {ErrS, MatchS, Reason} ->
            CloseCallback(TransC, PortC, TransS, PortS, {ErrS, PortS, Reason});
        {ErrC, MatchC, Reason} ->
            CloseCallback(TransC, PortC, TransS, PortS, {ErrC, PortC, Reason})
    after Timeout ->
        TimeoutCallback(TransC, PortC, TransS, PortS, timeout)
    end.

port(_Transport, Port) when is_port(Port) -> Port;
port(Transport, Port) ->
    %% this allows to override the port in messages for cases like
    %% ranch_proxy_protocol, which aims to look like ranch_tcp
    %% transparently while still holding state.
    case erlang:function_exported(Transport, match_port, 1) of
        true -> Transport:match_port(Port);
        false -> Port
    end.
