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
%%%-------------------------------------------------------------------
%% @copyright Heroku (2013-2015)
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc CommonTest test suite for vegur_client
%% @end
%%%-------------------------------------------------------------------

-module(vegur_client_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%
%%% Tests to run %%%
%%%%%%%%%%%%%%%%%%%%
%% Specific test cases or groups to run. The test case is named as
%% a single atom.

all() ->
    [{group, reason_phrases}
    ,{group, header_ordering}
    ,{group, timeouts}
    ].

groups() ->
    [{reason_phrases, [],
      [missing_reason_phrase
      ,deliberate_reason_phrase
      ,blank_reason_phrase]}
    ,{header_ordering, [],
      [order_preservation]}
    ,{timeouts, [],
      [first_read_timeout,
       other_read_timeout]}
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
    [{backend_port, Port} |Config];
init_per_testcase(deliberate_reason_phrase, Config) ->
    ranch:start_listener(deliberate_reason_phrase,
                         10,
                         ranch_tcp,
                         [{port, Port = 9988}],
                         ?MODULE,
                         [{handler, fun deliberate_reason_phrase_resp/2}]),
    [{backend_port, Port} | Config];
init_per_testcase(blank_reason_phrase, Config) ->
    ranch:start_listener(deliberate_reason_phrase,
                         10,
                         ranch_tcp,
                         [{port, Port = 9989}],
                         ?MODULE,
                         [{handler, fun blank_reason_phrase_resp/2}]),
    [{backend_port, Port} | Config];
init_per_testcase(Case = order_preservation, Config) ->
    Rules = cowboyku_router:compile([{'_',
                                      [{'_', header_ordering_handler, []}]}]),
    cowboyku:start_http(Case, 10,
                        [{port, Port = 9990}],
                        [{env, [{dispatch, Rules}]}]),
    [{backend_port, Port} | Config];
init_per_testcase(Case = first_read_timeout, Config) ->
    {ok, Original} = application:get_env(vegur, downstream_first_read_timeout),
    application:set_env(vegur, downstream_first_read_timeout, 0),
    Rules = cowboyku_router:compile([{'_',
                                      [{'_', timeout_handler, [first]}]}]),
    cowboyku:start_http(Case, 10,
                        [{port, Port = 9990}],
                        [{env, [{dispatch, Rules}]}]),
    [{backend_port, Port},
     {reset, [{downstream_first_read_timeout, Original}]}
    | Config];
init_per_testcase(Case = other_read_timeout, Config) ->
    {ok, Original} = application:get_env(vegur, idle_timeout),
    application:set_env(vegur, idle_timeout, 0),
    Rules = cowboyku_router:compile([{'_',
                                      [{'_', timeout_handler, [other]}]}]),
    cowboyku:start_http(Case, 10,
                        [{port, Port = 9990}],
                        [{env, [{dispatch, Rules}]}]),
    [{backend_port, Port},
     {reset, [{idle_timeout, Original}]}
    | Config];
init_per_testcase(_CaseName, Config) ->
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(Case = order_preservation, Config) ->
    cowboyku:stop_listener(Case),
    Config;
end_per_testcase(Case, Config) when Case =:= first_read_timeout; Case =:= other_read_timeout ->
    cowboyku:stop_listener(Case),
    Reset = ?config(reset, Config),
    [application:set_env(vegur, K, V) || {K, V} <- Reset],
    Config;
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
    backend_req(?config(backend_port, Config),
             421, <<"421 ">>).

deliberate_reason_phrase_resp(Socket, Transport) ->
    Transport:send(Socket,
                   <<"HTTP/1.1 421 Deliberate Phrase\r\n"
                     "Content-Length: 0\r\n"
                     "\r\n">>),
    Transport:close(Socket).

deliberate_reason_phrase(Config) ->
    backend_req(?config(backend_port, Config),
             421, <<"421 Deliberate Phrase">>).

blank_reason_phrase_resp(Socket, Transport) ->
    Transport:send(Socket,
                   <<"HTTP/1.1 421 \r\n"
                     "Content-Length: 0\r\n"
                     "\r\n">>),
    Transport:close(Socket).

blank_reason_phrase(Config) ->
    backend_req(?config(backend_port, Config),
             421, <<"421 ">>).

backend_req(Port, Code, Status) ->
    {ok, C1} = vegur_client:init([]),
    {ok, C2} = vegur_client:connect(ranch_tcp, {127,0,0,1}, Port, C1),
    Req = vegur_client:request_to_iolist(<<"GET">>, [], <<>>,
                                         'HTTP/1.1', <<"test.backend">>,
                                         <<"/">>),
    {ok, C3} = vegur_client:raw_request(Req, C2),
    {ok, Code, Status, _, C4} = vegur_client:response(C3),
    vegur_client:close(C4),
    ok.

order_preservation(Config) ->
    {ok, C1} = vegur_client:init([]),
    {ok, C2} = vegur_client:connect(ranch_tcp, {127,0,0,1},
                                    ?config(backend_port, Config), C1),
    Req = vegur_client:request_to_iolist(<<"GET">>, [], <<>>,
                                         'HTTP/1.1', <<"test.backend">>,
                                         <<"/">>),
    {ok, C3} = vegur_client:raw_request(Req, C2),
    {ok, 200, _, Headers, C4} = vegur_client:response(C3),
    [{<<"test-header">>, <<"1">>},
     {<<"test-header">>, <<"2">>},
     {<<"test-header">>, <<"3">>}] =
        [{K, V} || {K = <<"test-header">>, V} <- Headers],
    vegur_client:close(C4),
    ok.

first_read_timeout(Config) ->
    {ok, C1} = vegur_client:init([]),
    {ok, C2} = vegur_client:connect(ranch_tcp, {127,0,0,1},
                                    ?config(backend_port, Config), C1),
    Req = vegur_client:request_to_iolist(<<"GET">>, [], <<>>,
                                         'HTTP/1.1', <<"test.backend">>,
                                         <<"/">>),
    {ok, C3} = vegur_client:raw_request(Req, C2),
    T1 = os:timestamp(),
    %% Failing before any byte is received
    {error, timeout} = vegur_client:response(C3),
    T2 = os:timestamp(),
    ?assert((timer:now_diff(T2, T1) / 1000) < 250).

other_read_timeout(Config) ->
    {ok, C1} = vegur_client:init([]),
    {ok, C2} = vegur_client:connect(ranch_tcp, {127,0,0,1},
                                    ?config(backend_port, Config), C1),
    Req = vegur_client:request_to_iolist(<<"GET">>, [], <<>>,
                                         'HTTP/1.1', <<"test.backend">>,
                                         <<"/">>),
    {ok, C3} = vegur_client:raw_request(Req, C2),
    T1 = os:timestamp(),
    %% Failing after headers have been sent back
    {error, timeout} = vegur_client:response(C3),
    T2 = os:timestamp(),
    ?assert((timer:now_diff(T2, T1) / 1000) < 250).




start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, [{handler, F}]) ->
    ok = ranch:accept_ack(Ref),
    F(Socket, Transport).
