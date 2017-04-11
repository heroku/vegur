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
-module(vegur_req_log_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [merge].

init_per_testcase(merge, Config) ->
    Logs = [vegur_req_log:new(erlang:timestamp()) || _ <- lists:seq(1,5)],
    [{logs, list_to_tuple(Logs)} | Config].

end_per_testcase(merge, _Config) ->
    ok.

merge(Config) ->
    %% We create a crapload of events (5000) to be shared randomly across
    %% `N' different event logs, which we then merge down. All events
    %% should be kept, and all values should stay in order.
    Logs = ?config(logs, Config),
    N = tuple_size(Logs),
    EventCount = 5000,
    Events = [list_to_atom(integer_to_list(X)) || X <- lists:seq(1,EventCount)],
    Logs2 = lists:foldl(fun(Ev, AllLogs) ->
            timer:sleep(1),
            Pos = rand:uniform(N),
            Log = element(Pos, AllLogs),
            erlang:setelement(Pos, AllLogs, vegur_req_log:stamp(Ev, Log))
        end,
        Logs,
        Events),
    Merged = vegur_req_log:merge(tuple_to_list(Logs2)),
    %% All events kept in the right order
    AllStamps = [vegur_req_log:event(Ev, Merged) || Ev <- Events],
    ?assertEqual(AllStamps, lists:sort(AllStamps)),
    %% All events are in an absolute order in a flattened report. The keys are
    %% iolists, so we need to compare them the same. The second to last event
    %% is tricky and uses "_until_end" as a suffix -- the last one has "total".
    [{"init", _} | Report] = vegur_req_log:linear_report(Merged),
    StoredEvents = [list_to_atom(hd(K)) || {K, _Time} <- Report, K =/= "init", K=/="total"],
    ?assertEqual(Events, StoredEvents).

