%%%-------------------------------------------------------------------
%% @copyright Heroku (2014)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc CommonTest test suite for vegur_client
%% @end
%%%-------------------------------------------------------------------

-module(vegur_client_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%
%%% Tests to run %%%
%%%%%%%%%%%%%%%%%%%%
%% Specific test cases or groups to run. The test case is named as
%% a single atom.

all() ->
    [missing_reason_phrase
    ,deliberate_reason_phrase
    ,blank_reason_phrase
    ].

%%%%%%%%%%%%%%%%%%%%%%
%%% Setup/Teardown %%%
%%%%%%%%%%%%%%%%%%%%%%
%% Runs once at the beginning of the suite. The process is different
%% from the one the case will run in.
init_per_suite(Config) ->
    application:ensure_all_started(vegur),
    Config.

%% Runs once at the end of the suite. The process is different
%% from the one the case will run in.
end_per_suite(Config) ->
    Config.

%% Runs before the test case. Runs in the same process.
init_per_testcase(missing_reason_phrase, Config) ->
    ranch:start_listener(missing_reason_phrase,
                         10,
                         ranch_tcp,
                         [{port, Port = 9987}],
                         ?MODULE,
                         [{handler, fun missing_reason_phrase_resp/2}]),
    [{dyno_port, Port} |Config];
init_per_testcase(deliberate_reason_phrase, Config) ->
    ranch:start_listener(deliberate_reason_phrase,
                         10,
                         ranch_tcp,
                         [{port, Port = 9988}],
                         ?MODULE,
                         [{handler, fun deliberate_reason_phrase_resp/2}]),
    [{dyno_port, Port} | Config];
init_per_testcase(blank_reason_phrase, Config) ->
    ranch:start_listener(deliberate_reason_phrase,
                         10,
                         ranch_tcp,
                         [{port, Port = 9989}],
                         ?MODULE,
                         [{handler, fun blank_reason_phrase_resp/2}]),
    [{dyno_port, Port} | Config];
init_per_testcase(_CaseName, Config) ->
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(CaseName, Config) ->
    ranch:stop_listener(CaseName),
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%
missing_reason_phrase_resp(Socket, Transport) ->
    Transport:send(Socket,
                   <<"HTTP/1.1 421\r\n"
                     "Content-Length: 0\r\n"
                     "\r\n">>),
    Transport:close(Socket).

missing_reason_phrase(Config) ->
    dyno_req(?config(dyno_port, Config),
             421, <<"421 Unknown">>).

deliberate_reason_phrase_resp(Socket, Transport) ->
    Transport:send(Socket,
                   <<"HTTP/1.1 421 Deliberate Phrase\r\n"
                     "Content-Length: 0\r\n"
                     "\r\n">>),
    Transport:close(Socket).

deliberate_reason_phrase(Config) ->
    dyno_req(?config(dyno_port, Config),
             421, <<"421 Deliberate Phrase">>).

blank_reason_phrase_resp(Socket, Transport) ->
    Transport:send(Socket,
                   <<"HTTP/1.1 421 \r\n"
                     "Content-Length: 0\r\n"
                     "\r\n">>),
    Transport:close(Socket).

blank_reason_phrase(Config) ->
    dyno_req(?config(dyno_port, Config),
             421, <<"421 ">>).

dyno_req(Port, Code, Status) ->
    {ok, C1} = vegur_client:init([]),
    {ok, C2} = vegur_client:connect(ranch_tcp, {127,0,0,1}, Port, C1),
    Req = vegur_client:request_to_iolist(<<"GET">>, [], <<>>,
                                         'HTTP/1.1', <<"test.dyno">>,
                                         <<"/">>),
    {ok, C3} = vegur_client:raw_request(Req, C2),
    {ok, Code, Status, _, C4} = vegur_client:response(C3),
    vegur_client:close(C4),
    ok.

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, [{handler, F}]) ->
    ok = ranch:accept_ack(Ref),
    F(Socket, Transport).
