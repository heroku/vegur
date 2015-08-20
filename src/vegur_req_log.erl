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
%%%
%% @copyright Fred Hebert
%% @author Fred Hebert <mononcqc@ferd.ca>
%% @version {@vsn}, {@date} {@time}
%% @doc Request event log processing structure
%% @end
-module(vegur_req_log).

-record(log, {start :: erlang:timestamp(),
              events = queue:new()}).

-export([new/1
        ,start_time/1
        ,start_to_proxy_duration/1
        ,connect_duration/1
        ,log/3
        ,stamp/2
        ,stamp/3
        ,stamp/4
        ,timestamp_diff/3
        ,timestamp_diff/4
        ,record/4
        ,linear_report/1
        ,linear_report/2
        ,linear_summary/1
        ,flatten_report/1
        ,event_duration/2
        ,event/2
        ,event/3
        ,merge/1
        ]).


-opaque request_log() :: #log{}.
-type event_type() :: atom().
-type details() :: term().
-type ms() :: non_neg_integer().

-export_type([request_log/0, event_type/0, details/0]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
%% Starts a new log
-spec new(erlang:timestamp()) -> request_log().
new({_,_,_} = StartTime) ->
    #log{start = StartTime}.

%% Returns start time from req log
-spec start_time(request_log()) -> erlang:timestamp().
start_time(#log{start = StartTime}) ->
    StartTime.

-spec start_to_proxy_duration(request_log()) -> ms().
start_to_proxy_duration(Log) ->
    StartTime = start_time(Log),
    PreProxy = vegur_req_log:event(pre_proxy, Log),
    timer:now_diff(PreProxy, StartTime) div 1000.

-spec connect_duration(request_log()) -> ms().
connect_duration(Log) ->
    event_duration(connect_time, Log).

%% Log a given event. The log of an event results in a stamp added right before
%% and right after the event (wrapped as a 0-argument fun).
-spec log(event_type(), fun(() -> term()), request_log()) -> {term(), request_log()}.
log(EventType, Fun, Log = #log{}) when is_function(Fun, 0) ->
    Start = os:timestamp(),
    Ret = Fun(),
    End = os:timestamp(),
    NewLog = record(Start, End, EventType, Log),
    {Ret, NewLog}.

%% A stamp is a single entry for the event log, that may or may not have a
%% specific note attached with it. An entry with no note gets it replaced
%% with 'no_details'
-spec stamp(event_type(), request_log()) -> request_log().
stamp(EventType, Log) ->
    stamp(EventType, no_details, Log).

-spec stamp(event_type(), erlang:timestamp() | details(), request_log()) -> request_log().
stamp(EventType, T={_,_,_}, Log) ->
    stamp(EventType, T, no_details, Log);

stamp(EventType, Details, Log) ->
    T = os:timestamp(),
    stamp(EventType, T, Details, Log).

-spec stamp(event_type(), erlang:timestamp(), details(), request_log()) -> request_log().
stamp(EventType, T, Details, Log = #log{events = Q}) ->
    Log#log{events = queue:in({T, EventType, Details}, Q)}.

-spec timestamp_diff(event_type(), event_type(), request_log()) -> term().
timestamp_diff(BeginningEventType, EndingEventType, Log) ->
    timestamp_diff(BeginningEventType, EndingEventType, undefined, Log).

%% returns a millisecond diff between two event_types if there exists a
%% single stamp for each event_type
-spec timestamp_diff(event_type(), event_type(), term(), request_log()) -> term().
timestamp_diff(BeginningEventType, EndingEventType, Default, Log) ->
    case {fetch_event_stamps(BeginningEventType, Log),
          fetch_event_stamps(EndingEventType, Log)} of
        {[T1], [T2]} when is_tuple(T1), is_tuple(T2) ->
            timer:now_diff(T2, T1) div 1000;
        _ ->
            Default
    end.

%% Allows to log a given event. Works in a manner similar to log/3, except that
%% the user may pick two arbitrary points in time to stamp. Both events will
%% be stamped under the same EventType, but the note of one will say 'pre' for
%% the StartTime, and 'post' for the EndTime.
-spec record(Start::erlang:timestamp(), End::erlang:timestamp(),
             event_type(), request_log()) -> request_log().
record(StartTime = {_,_,_}, EndTime = {_,_,_},
       EventType, Log = #log{events = Q}) ->
    Q1 = queue:in({StartTime, EventType, pre}, Q),
    Q2 = queue:in({EndTime, EventType, post}, Q1),
    Log#log{events = Q2}.

%% Creates a linear summary of entries in the event log. It means that for times
%% to be meaningful, there should ideally not be overlapping intervals included
%% as part of the log.
-spec linear_summary(request_log()) ->
    {[{{Start::event_type(),End::event_type()}, ms()}], {total, ms()}}.
linear_summary(#log{start = StartTime, events = Events}) ->
    Now = os:timestamp(),
    %% We need a stable sorting function for events that are close together
    %% not to be swapped in time.
    Sort = fun({StartA,_,_},{StartB,_,_}) -> StartA =< StartB end,
    summarize_events(lists:sort(
            Sort,
            queue:to_list(queue:in_r(
                    {StartTime, '$start', no_details},
                    queue:in({Now, '$stop', no_details}, Events)))
        ), StartTime, Now).

%% Creates a linear repport of entries in the event log. Its output is rather
%% similar to linear_summary/1, but the events are tentatively named in
%% a relevant way, and as iolists ready to be output
-spec linear_report(request_log()) -> [{Key::iodata(), ms()}].
linear_report(Log = #log{}) ->
    {Metrics, {total,Total}} = linear_summary(Log),
    [case Metric of
        {{'$start',_}, T}    -> {"init", T};
        {{Name,Name}, T}     -> {atom_to_list(Name), T};
        {{Name, '$stop'}, T} -> {[atom_to_list(Name), "_until_end"], T};
        {{NameA, NameB}, T}  -> {[atom_to_list(NameA), "_to_", atom_to_list(NameB)], T}
    end || Metric <- Metrics] ++ [{"total",Total}].

%% Meaning we need to filter for events we want. This function is similar to
%% linear_report/1 except it allows to filter entries based on their
%% event_type() to allow customization of returned values over time.
-spec linear_report(fun((event_type()) -> boolean()), request_log()) -> [{Key::iodata(), ms()}].
linear_report(Pred, Log = #log{events=Events}) ->
    %% The internal format is different from the external one, so we wrap
    %% things around to create a proper abstraction for users. We drop the time
    %% and also drop the details. We may want to expand this later but there is
    %% no need for it at this point.
    Filter = fun({_, Item, _}) -> Pred(Item) end,
    linear_report(Log#log{events=queue:filter(Filter,Events)}).

%% Move from
%%
%%  init=0ms resolve=0ms resolve_to_lookup_service=0ms lookup_service=0ms
%%  lookup_service_to_resolve=5006ms resolve=0ms resolve_to_lookup_service=0ms
%%  lookup_service=0ms lookup_service_to_resolve=1ms resolve=0ms
%%  resolve_to_lookup_service=0ms lookup_service=0ms
%%  lookup_service_to_connection=0ms connection=1ms connection_to_headers_sent=0ms
%%  headers_sent_to_first_packet=3ms first_packet_until_end=1ms total=5014ms
%%
%% to:
%%
%%  init=0ms resolve=0ms resolve_to_lookup_service=0ms lookup_service=0ms
%%  lookup_service_to_resolve=5007ms lookup_service_to_connection=0ms
%%  connection=1ms connection_to_headers_sent=0ms headers_sent_to_first_packet=3ms
%%  first_packet_until_end=1ms total=5014ms
%%
%% which basically collapse events with the same key into one
-spec flatten_report([{Key::iodata(), ms()}]) -> [{Key::iodata(), ms()}].
flatten_report(Report) ->
    flatten_report(Report, []).

flatten_report([], Flat) ->
    Flat;
flatten_report([{K,V}|Report], Flat) ->
    flatten_report(Report, add({K,V}, Flat)).

-spec event_duration(Name, Log) ->
                            ms()|undefined when
      Name :: event_type(),
      Log :: request_log().
event_duration(Name, #log{events=Events}) ->
    case event_find(Name, queue:to_list(Events), []) of
        [{Stamp1,_,pre},{Stamp2,_,post}] -> timer:now_diff(Stamp2,Stamp1) div 1000;
        [{Stamp1,_,post},{Stamp2,_,pre}] -> timer:now_diff(Stamp1,Stamp2) div 1000;
        _ -> undefined
    end.

-spec event(Name, Log) ->
        erlang:timestamp()|undefined when
      Name :: event_type(),
      Log :: request_log().
event(Name, Log) ->
    event(Name, Log, undefined).

-spec event(Name, Log, Default) ->
                   ms()|Default when
      Name :: event_type(),
      Log :: request_log(),
      Default :: term().
event(EventType, Log, Default) ->
    case fetch_event_stamps(EventType, Log) of
        [T1] ->
            T1;
        List = [_|_] -> % pick the newest one
            hd(lists:reverse(List));
        _ ->
            Default
    end.

event_find(_Name, [], Acc) -> Acc;
event_find(Name, [Tag={_,Name,_}|Rest], Acc) ->
    case Acc of
        [] -> event_find(Name, Rest, [Tag]);
        [_] -> [Tag | Acc]
    end;
event_find(Name, [_|Rest], Acc) -> event_find(Name, Rest, Acc).

-spec merge([request_log(), ...]) -> request_log().
merge([Log]) -> Log;
merge([Log1,Log2|Logs]) ->
    merge([merge(Log1,Log2)|Logs]).

merge(#log{start=Start1, events=Ev1}, #log{start=Start2, events=Ev2}) ->
    Events = events_merge(queue:to_list(Ev1), queue:to_list(Ev2), queue:new()),
    #log{start=min(Start1,Start2), events=Events}.


events_merge([], [], Queue) ->
    Queue;
events_merge([], [B|Bs], Queue) ->
    events_merge([], Bs, queue:in(B, Queue));
events_merge([A|As], [], Queue) ->
    events_merge(As, [], queue:in(A, Queue));
events_merge([A={TimeA, _, _}|RestA], [B={TimeB, _, _}|RestB], Queue) ->
    if A =:= B         -> events_merge(RestA, RestB, queue:in(A, Queue))
     ; TimeA < TimeB   -> events_merge(RestA, [B|RestB], queue:in(A, Queue))
     ; TimeA > TimeB   -> events_merge([A|RestA], RestB, queue:in(B, Queue))
     %% Both timestamps are equal, sort in fancy ways. That's due to the fact
     %% Multiple queues and events may, given we use os:timestamp(), end up
     %% running on cores or at times where CPUs appear to go back in time when
     %% it comes to µs. Try to fix up some event merges that are obvious.
     ; TimeA =:= TimeB ->
        case {A,B} of
            {{_,Type,pre},{_,Type,post}} ->
                events_merge(RestA, [B|RestB], queue:in(A, Queue));
            {{_,Type,post},{_,Type,pre}} ->
                events_merge([A|RestA], RestB, queue:in(B, Queue));
            _ when A < B ->
                events_merge(RestA, [B|RestB], queue:in(A, Queue));
            _ when A > B ->
                events_merge([A|RestA], RestB, queue:in(B, Queue))
        end
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%
-spec fetch_event_stamps(event_type(), request_log()) -> [erlang:timestamp()].
fetch_event_stamps(EventType1, #log{events = Q}) ->
    [T || {T, EventType2, _Details} <- queue:to_list(Q), EventType1 =:= EventType2].

summarize_events(List, Start, End) ->
    {intervals(List), {total,time_val(Start,End)}}.

intervals([_]) ->
    [];
intervals([{T1,Event1,_},E2={T2,Event2,_} |Rest]) ->
    [{{Event1,Event2}, time_val(T1,T2)} | intervals([E2|Rest])].

%% never go negative
time_val(T1, T2) ->
    case round(timer:now_diff(T2, T1)/1000) of
        N when N < 0 -> 0;
        N -> N
    end.

%% the function preserves original order and merges values by adding them
add({K,V}, []) -> [{K,V}];
add({K,V}, [{K,OldV}|Acc]) -> [{K,V+OldV}|Acc];
add({K,V}, [Entry|Acc]) -> [Entry|add({K,V},Acc)].
