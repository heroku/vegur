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
-module(vegur_proxy_middleware).

-behaviour(cowboyku_middleware).
-export([execute/2]).

-record(state, { backend_client :: vegur_client:client()
                 ,env
               }).

execute(Req, Env) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    Log1 = vegur_req_log:stamp(pre_proxy, Log),
    Req2 = cowboyku_req:set_meta(logging, Log1, Req1),
    {Client, Req3} = cowboyku_req:meta(backend_connection, Req2),
    case vegur_req_log:log(service_time,
                           fun() ->
                                   proxy(Req3, #state{backend_client = Client, env = Env})
                           end, Log1) of
        {{ok, Code, _Status, Req4, #state{backend_client=Client1}}, Log2} ->
            Req5 = store_byte_counts(Req4, Client1),
            Req6 = cowboyku_req:set_meta(status, successful, Req5),
            {_, Req7} = merge_logs(Log2, Req6, Client1),
            {halt, Code, Req7};
        {{error, Blame, Reason, Req4}, Log2} ->
            {HttpCode, Req5} = vegur_utils:handle_error({Blame, Reason}, Req4),
            Req6 = store_byte_counts(Req5, Client),
            {_, Req7} = merge_logs(Log2, Req6, Client),
            {error, HttpCode, Req7}
    end.

proxy(Req, State) ->
    case parse_request(Req) of
        {BackendReq, Req1} ->
            send_to_backend(BackendReq, Req1, State);
        {error, Blame, Error} ->
            {error, Blame, Error, Req}
    end.

send_to_backend({Method, Header, Body, Path, Url}=Request, Req,
                #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:send_headers(Method, Header, Body, Path, Url, Req, BackendClient) of
        {done, Req1, BackendClient1} -> % headers sent
            {Log, Req2} = cowboyku_req:meta(logging, Req1),
            Log1 = vegur_req_log:stamp(headers_sent, Log),
            Req3 = cowboyku_req:set_meta(logging, Log1, Req2),
            send_body_to_backend(Request, Req3, State#state{backend_client=BackendClient1});
        {ok, Code, Status, RespHeaders, BackendClient1} -> % request ended without body sent
            {Log, Req1} = cowboyku_req:meta(logging, Req),
            Log1 = vegur_req_log:stamp(headers_sent, Log),
            Req2 = cowboyku_req:set_meta(logging, Log1, Req1),
            handle_backend_response(Code, Status, RespHeaders, Req2,
                                    State#state{backend_client=BackendClient1});
        {error, Blame, Error} ->
            {error, Blame, Error, Req}
    end.

send_body_to_backend({Method, Header, Body, Path, Url}, Req,
                     #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:send_body(Method, Header, Body, Path, Url, Req, BackendClient) of
        {done, Req1, BackendClient1} ->
            read_backend_response(Req1, State#state{backend_client=BackendClient1});
        {error, Blame, Error} ->
            {error, Blame, Error, store_byte_counts(Req, BackendClient)}
    end.

read_backend_response(Req, #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:read_backend_response(Req, BackendClient) of
        {ok, Code, Status, RespHeaders, Req1, BackendClient1} ->
            handle_backend_response(Code, Status, RespHeaders, Req1,
                                    State#state{backend_client=BackendClient1});
        {error, Blame, Error} ->
            {error, Blame, Error, store_byte_counts(Req, BackendClient)}
    end.

handle_backend_response(Code, Status, RespHeaders, Req, State) ->
    Req1 = cowboyku_req:set_meta(response_headers, RespHeaders, Req),
    {Type, Req2} = cowboyku_req:meta(request_type, Req1, []),
    case lists:sort(Type) of
        [] ->
            http_request(Code, Status, RespHeaders, Req2, State);
        [upgrade] ->
            upgrade_request(Code, Status, RespHeaders, Req2, State)
    end.

upgrade_request(101, Status, Headers, Req, #state{backend_client=BackendClient}=State) ->
    {Result, Req1, BackendClient1} = vegur_proxy:upgrade(Headers, Req, BackendClient),
    case Result of
        timeout ->
            {error, undefined, timeout, store_byte_counts(Req1, BackendClient1)};
        done ->
            {ok, 101, Status, Req1, State#state{backend_client=BackendClient1}}
    end;
upgrade_request(Code, Status, Headers, Req, State) ->
    http_request(Code, Status, Headers, Req, State).

http_request(Code, Status, Headers, Req,
             #state{backend_client=BackendClient}=State) ->
    case vegur_proxy:relay(Code, Status, Headers, Req, BackendClient) of
        {ok, Req1, BackendClient1} ->
            {ok, Code, Status, Req1, State#state{backend_client=BackendClient1}};
        {error, Blame, Error, Req1} ->
            {error, Blame, Error, store_byte_counts(Req1, BackendClient)}
    end.

store_byte_counts(Req, Client) ->
    {SentNew, RecvNew} = vegur_client:byte_counts(Client),
    {BytesSent, Req2} = cowboyku_req:meta(bytes_sent, Req),
    {BytesRecv, Req3} = cowboyku_req:meta(bytes_recv, Req2),
    Sent = case {BytesSent, SentNew} of
        %{_, undefined} -> BytesSent; % dialyzer says this can't happen
        {undefined, _} -> SentNew;
        {_,_} -> max(BytesSent, SentNew)
    end,
    Recv = case {BytesRecv, RecvNew} of
        %{_, undefined} -> BytesRecv; % dialyzer says this can't happen
        {undefined, _} -> RecvNew;
        {_,_} -> max(BytesRecv, RecvNew)
    end,
    Req4 = cowboyku_req:set_meta(bytes_sent, Sent, Req3),
    cowboyku_req:set_meta(bytes_recv, Recv, Req4).

%% We need to merge the logs obtained in the request with those tracked
%% internally in the req object, and those tracked in the client.
%% There is a tricky bit here where because vegur_req_log is called
%% outside of the proxying functions, it's possible its contents will
%% be different to what is called inside of it, so we need to do a 3-way
%% merge.
merge_logs(Log, Req, Client) ->
    {ReqLog, Req2} = cowboyku_req:meta(logging, Req),
    Merged = vegur_req_log:merge([Log, ReqLog, vegur_client:log(Client)]),
    {Merged, cowboyku_req:set_meta(logging, Merged, Req2)}.

parse_request(Req) ->
    case check_for_body(Req) of
        {{error, Reason}, _Req} ->
            {error, upstream, Reason};
        {Body, Req1} ->
            parse_request(Body, Req1)
    end.

parse_request(Body, Req) ->
    {Method, Req2} = cowboyku_req:method(Req),
    {Path, Req3} = cowboyku_req:path(Req2),
    {Host, Req4} = cowboyku_req:host(Req3),
    {Qs, Req5} = cowboyku_req:qs(Req4),
    {Headers, Req6} = cowboyku_req:headers(Req5),
    FullPath = case Qs of
        <<>> -> Path;
        _ -> <<Path/binary, "?", Qs/binary>>
    end,
    {Headers2, Req7} = add_proxy_headers(Headers, Req6),
    {{Method, Headers2, Body, FullPath, Host}, Req7}.

add_proxy_headers(Headers, Req) ->
    {Headers1, Req1} = add_request_id(Headers, Req),
    {Headers2, Req2} = add_forwarded(Headers1, Req1),
    {Headers3, Req3} = add_via(Headers2, Req2),
    {Headers4, Req4} = add_connect_time(Headers3, Req3),
    {Headers5, Req5} = add_start_time(Headers4, Req4),
    {Headers6, Req6} = add_total_route_time(Headers5, Req5),
    add_upstream_interface_headers(Headers6, Req6).

add_upstream_interface_headers(Headers, Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    {Headers1, Req2} = vegur_utils:add_interface_headers(upstream, Headers, Req1),
    Log1 = vegur_req_log:stamp(headers_formatted, Log),
    Req3 = cowboyku_req:set_meta(logging, Log1, Req2),
    {Headers1, Req3}.

add_start_time(Headers, Req) ->
    {Time, Req1} = vegur_req:start_time(Req),
    {vegur_utils:add_or_replace_header(vegur_utils:config(start_time_header),
                                       integer_to_list(timer:now_diff(Time, {0,0,0}) div 1000),
                                       Headers),
     Req1}.

add_connect_time(Headers, Req) ->
    {Time, Req1} = vegur_request_log:get_log_value(connect_time, Req),
    {vegur_utils:add_or_replace_header(vegur_utils:config(connect_time_header),
                                       integer_to_list(Time), Headers),
     Req1}.

add_total_route_time(Headers, Req) ->
    {Time, Req1} =
        case vegur_request_log:total_routing_time(Req) of
            {undefined, Req2} ->
                {"null", Req2};
            {Time1, Req2} ->
                {integer_to_list(Time1), Req2}
        end,
    {vegur_utils:add_or_replace_header(vegur_utils:config(route_time_header),
                                       Time, Headers), Req1}.

add_request_id(Headers, Req) ->
    {RequestId, Req1} = cowboyku_req:meta(request_id, Req),
    {vegur_utils:add_or_replace_header(vegur_utils:config(request_id_name), RequestId, Headers),
     Req1}.

add_forwarded(Headers, Req) ->
    {Headers1, Req2} = case vegur_utils:peer_ip_port(Req) of
                           {{PeerAddress, PeerPort, DestPort}, Req1} ->
                               handle_feature(Req1, {Headers, PeerPort});
                           {{PeerAddress, DestPort}, Req1} ->
                               {Headers, Req1}
                       end,

    {Headers2, Req3} = vegur_utils:add_or_append_header(<<"x-forwarded-for">>, inet:ntoa(PeerAddress), Headers1, Req2),

    Headers3 =
        case DestPort of
            80 ->
                vegur_utils:add_or_replace_header(<<"x-forwarded-proto">>, <<"http">>, Headers2);
            443 ->
                vegur_utils:add_or_replace_header(<<"x-forwarded-proto">>, <<"https">>, Headers2);
            _ ->
                Headers2
        end,

    Headers4 = vegur_utils:add_or_replace_header(<<"x-forwarded-port">>, integer_to_list(DestPort), Headers3),

    {Headers4, Req3}.

add_via(Headers, Req) ->
    Via = vegur_utils:get_via_value(),
    vegur_utils:add_or_append_header(<<"via">>, Via, Headers, Req).

handle_feature(Req, {Headers, PeerPort}) ->
    {InterfaceModule, HandlerState, Req1} = vegur_utils:get_interface_module(Req),
    case InterfaceModule:feature(peer_port, HandlerState) of
        {enabled, HandlerState2} ->
            Req2 = vegur_utils:set_handler_state(HandlerState2, Req1),
            {vegur_utils:add_or_replace_header(<<"x-forwarded-peer-port">>,
                                              integer_to_list(PeerPort),
                                               Headers), Req2};
        {disabled, HandlerState2} ->
            Req2 = vegur_utils:set_handler_state(HandlerState2, Req1),
            {Headers, Req2}
    end.

check_for_body(Req) ->
    %% We handle the request differently based on whether it's chunked,
    %% has a known length, or if it has no body at all.
    case cowboyku_req:has_body(Req) of
        true ->
            case cowboyku_req:body_length(Req) of
                {undefined, Req2} ->
                    {{stream, chunked}, Req2};
                {error, badarg} ->
                    {{error, invalid_transfer_encoding}, Req};
                {Length, Req2} ->
                    {{stream, Length}, Req2}
            end;
        false ->
            {<<>>, Req}
    end.
