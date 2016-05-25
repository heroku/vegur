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
-module(vegur_utils).

-define(APP, vegur).

-export([get_interface_module/1
         ,set_handler_state/2
         ,set_service_state/2
         ,parse_header/2
         ,add_or_append_header/3
         ,add_or_append_header/4
         ,add_if_missing_header/4
         ,add_or_replace_header/3
         ,add_or_replace_headers/2
         ,delete_all_headers/2
         ,set_request_status/2
         ,get_request_status/1
         ,add_interface_headers/3
         ,handle_error/2
         ,peer_ip_port/1
         ,connection_info/1
         ,connection_info/2
         ,borrow_cowboyku_socket/1
         ,raw_cowboyku_socket/1
         ,raw_cowboyku_sockbuf/1
         ,append_to_cowboyku_buffer/2
         ,mark_cowboyku_close/1
        ]).

-export([config/1
         ,config/2
         ,get_via_value/0]).

-spec get_interface_module(Req) ->
                                  {Module, HandlerState, Req}
                                      | no_return() when
      Req :: cowboyku_req:req(),
      HandlerState :: term(),
      Module :: module().
get_interface_module(Req) ->
    {HandlerState, Req1} = cowboyku_req:meta(handler_state, Req, undefined),
    {vegur_utils:config(interface_module), HandlerState, Req1}.

-spec set_handler_state(HandlerState, Req) -> Req when
      HandlerState :: term(),
      Req :: cowboyku_req:req().
set_handler_state(HandlerState, Req) ->
    cowboyku_req:set_meta(handler_state, HandlerState, Req).

-spec set_service_state(ServiceState, Req) -> Req when
      ServiceState :: term(),
      Req :: cowboyku_req:req().
set_service_state(ServiceState, Req) ->
    cowboyku_req:set_meta(service_state, ServiceState, Req).

-spec parse_header(binary(), cowboyku_req:req()) ->
                          {ok, {[]|[binary()|undefined], cowboyku_req:req()}}
                        | {error, badarg}.
parse_header(Key, Req) ->
    case cowboyku_req:parse_header(Key, Req) of
        {ok, L, Req0} when is_list(L) -> {ok, {L, Req0}};
        {ok, Term, Req0} -> {ok, {[Term], Req0}};
        {undefined, Term, Req0} ->  {ok, {[Term], Req0}};
        {error, badarg} -> {error, badarg}
    end.


%% @doc adds a given header when not present, otherwise appends the header
%% value to the end of the header list, and uses the HTTP's comma-separated
%% header values to keep everything present. For exmaple, appending
%% 'via: 1.1 vegur' to an existing 'via: 1.1 example' would yield
%% 'via: 1.1 example, 1.1 vegur'
-spec add_or_append_header(Key, Value, Headers) -> Headers when
      Key :: iodata(),
      Value :: iodata(),
      Headers :: [{iodata(), iodata()}]|[].
add_or_append_header(Key, Val, Headers) ->
    case lists:keyfind(Key, 1, Headers) of
        false ->
            Headers ++ [{Key, Val}];
        {_Key, Current} ->
            lists:keyreplace(Key, 1, Headers, {Key, [Current, ", ", Val]})
    end.

%% @doc Similar to add_or_append_header/3, but takes a `Req' object to
%% allow the reading of the current header value to be done through
%% Cowboyku's cache.
-spec add_or_append_header(Key, Value, Headers, Req) ->
                                  {Headers, Req} when
      Key :: iodata(),
      Value :: iodata(),
      Headers :: [{iodata(), iodata()}]|[],
      Req :: cowboyku_req:req().
add_or_append_header(Key, Val, Headers, Req) ->
    case cowboyku_req:header(Key, Req) of
        {undefined, Req2} ->
            {Headers ++ [{Key, Val}], Req2};
        {CurrentVal, Req2} ->
            {lists:keyreplace(Key, 1, Headers, {Key, [CurrentVal, ", ", Val]}),
             Req2}
    end.

-spec add_if_missing_header(Key, Value, Headers, Req) ->
                                   {Headers, Req} when
      Key :: iodata(),
      Value :: iodata(),
      Headers :: [{iodata(), iodata()}]|[],
      Req :: cowboyku_req:req().
add_if_missing_header(Key, Val, Headers, Req) ->
    {NewVal, Req2} =
        case cowboyku_req:header(Key, Req) of
            {undefined, Req1} ->
                {Val, Req1};
            {CurrentVal, Req1} ->
                {CurrentVal, Req1}
        end,
    {Headers ++ [{Key, NewVal}], Req2}.

-spec add_or_replace_header(Key, Value, Headers) ->
                                    Headers when
      Key :: iodata(),
      Value :: iodata(),
      Headers :: [{iodata(), iodata()}]|[].
add_or_replace_header(Key, Value, Headers) ->
    lists:keystore(Key, 1, Headers, {Key, Value}).

-spec add_or_replace_headers(AdditionalHeaders, Headers) ->
                                    Headers when
      Key :: iodata(),
      Value :: iodata(),
      AdditionalHeaders :: [{Key, Value}],
      Headers :: [{iodata(), iodata()}]|[].
add_or_replace_headers(AdditionalHeaders, Headers) ->
    lists:keymerge(1, AdditionalHeaders, Headers).

%% We need to traverse the entire list because a user could have
%% injected more than one instance of the same header, and cowboyku
%% doesn't coalesce headers for us.
-spec delete_all_headers(Key, Headers) -> Headers when
      Key :: iodata(),
      Headers :: [{iodata(), iodata()}].
delete_all_headers(_, []) -> [];
delete_all_headers(Key, [{Key,_} | Hdrs]) -> delete_all_headers(Key, Hdrs);
delete_all_headers(Key, [H|Hdrs]) -> [H | delete_all_headers(Key, Hdrs)].

-spec set_response(Headers, Body, Req) ->
                             Req when
      Headers :: [{iodata(), iodata()}]|[],
      Body :: binary(),
      Req :: cowboyku_req:req().
set_response(Headers, Body, Req) ->
    Req1 = cowboyku_req:set_resp_body(Body, Req),
    lists:foldl(fun({Name, Value}, R) ->
                        cowboyku_req:set_resp_header(Name, Value, R)
                end, Req1, Headers).

-spec set_request_status(Status, Req) -> Req when
      Status :: vegur_interface:terminate_reason(),
      Req :: cowboyku_req:req().
set_request_status(Status, Req) ->
    cowboyku_req:set_meta(status, Status, Req).

-spec get_request_status(Req) -> {Status, Req} when
      Status :: vegur_interface:terminate_reason(),
      Req :: cowboyku_req:req().
get_request_status(Req) ->
    cowboyku_req:meta(status, Req).

-spec add_interface_headers(Direction, Headers, Req) -> {Headers, Req} when
    Direction :: upstream | downstream,
    Headers :: [{iodata(), iodata()}],
    Req :: cowboyku_req:req().
add_interface_headers(Direction, Headers, Req) ->
    {Log, Req1} = cowboyku_req:meta(logging, Req),
    {InterfaceModule, HandlerState, Req2} = get_interface_module(Req1),
    {InterfaceHeaders, HandlerState1} = InterfaceModule:additional_headers(Direction, Log, Req2, HandlerState),
    Req3 = set_handler_state(HandlerState1, Req2),
    {add_or_replace_headers(InterfaceHeaders, Headers), Req3}.

-spec handle_error(Reason, Req) -> {HttpCode, Req} when
      Reason :: atom() | {Blame::atom(), term()},
      HttpCode :: cowboyku:http_status(),
      Req :: cowboyku_req:req().
handle_error(Reason, Req) ->
    {InterfaceModule, HandlerState, Req1} = get_interface_module(Req),
    {DomainGroup, Req2} = cowboyku_req:meta(domain_group, Req1, undefined),
    {{HttpCode, ErrorHeaders, ErrorBody}, Req3, HandlerState1} = InterfaceModule:error_page(Reason, DomainGroup, Req2, HandlerState),
    Req4 = set_handler_state(HandlerState1, Req3),
    Req5 = set_response(ErrorHeaders, ErrorBody, Req4),
    Req6 = set_request_status(error, Req5),
    Req7 = set_service_state(Reason, Req6),
    {HttpCode, Req7}.



-spec peer_ip_port(Req) -> {{IpAddress, PortNumber}|
                            {IpAddress, PortNumber, PortNumber}, Req} when
      IpAddress :: inet:ip_address(),
      PortNumber :: inet:port_number(),
      Req :: cowboyku_req:req().
peer_ip_port(Req) ->
    Transport = cowboyku_req:get(transport, Req),
    case Transport:name() of
        proxy_protocol_tcp ->
            ProxySocket = cowboyku_req:get(socket, Req),
            {ok, {{PeerIp, PeerPort}, {_, DestPort}}} = Transport:proxyname(ProxySocket),
            {{PeerIp, PeerPort, DestPort}, Req};
        proxy_protocol_ssl ->
            ProxySocket = cowboyku_req:get(socket, Req),
            {ok, {{PeerIp, PeerPort}, {_, DestPort}}} = Transport:proxyname(ProxySocket),
            {{PeerIp, PeerPort, DestPort}, Req};
        _ ->
            {{PeerIp, _}, Req3} = cowboyku_req:peer(Req),
            {Port, Req4} = cowboyku_req:port(Req3),
            {{PeerIp, Port}, Req4}
    end.

-spec connection_info(Req) -> {list(), Req} when
      Req :: cowboyku_req:req().
connection_info(Req) ->
    connection_info([protocol, cipher_suite, sni_hostname], Req).

-spec connection_info([protocol | cipher_suite | sni_hostname], Req) -> {list(), Req} when
      Req :: cowboyku_req:req().
connection_info(Items, Req) ->
    Transport = cowboyku_req:get(transport, Req),
    case Transport:name() of
        proxy_protocol_tcp ->
            ProxySocket = cowboyku_req:get(socket, Req),
            {ok, Ret} = Transport:connection_info(ProxySocket, Items),
            {Ret, Req};
        proxy_protocol_ssl ->
            ProxySocket = cowboyku_req:get(socket, Req),
            {ok, Ret} = Transport:connection_info(ProxySocket, Items),
            {Ret, Req};
        _ ->
            {[], Req}
    end.

%% Get a cowboyku socket for a temporary operation, expected to be
%% non-destructive and non-terminal (more data to be sent by the
%% regular cowboyku code path after).
-spec borrow_cowboyku_socket(Req) -> {Transport, Socket} when
    Transport :: module(),
    Socket :: any(),
    Req :: cowboyku_req:req().
borrow_cowboyku_socket(Req) ->
    [Transport, Socket] = cowboyku_req:get([transport, socket], Req),
    {Transport, Socket}.

%% Get a cowboyku socket for a terminal operation, expected to be
%% destructive (no more data to be sent by the regular cowboyku code path after)
-spec raw_cowboyku_socket(Req) ->  {{Transport, Socket}, Req} when
    Transport :: module(),
    Socket :: any(),
    Req :: cowboyku_req:req().
raw_cowboyku_socket(Req) ->
    [Transport, Socket] = cowboyku_req:get([transport, socket], Req),
    {{Transport, Socket}, mark_as_done(Req)}.

%% Get a cowboyku socket for a terminal operation, expected to be
%% destructive (no more data to be sent by the regular cowboyku code path after).
%% To be used specifically when the currently buffered data is relevant and
%% needs to be used.
-spec raw_cowboyku_sockbuf(Req) -> {{Transport, Socket}, Buffer, Req} when
    Transport :: module(),
    Socket :: any(),
    Buffer :: iodata(),
    Req :: cowboyku_req:req().
raw_cowboyku_sockbuf(Req) ->
    [Transport, Socket, Buffer] = cowboyku_req:get([transport, socket, buffer], Req),
    {{Transport, Socket},
     Buffer,
     mark_as_done(cowboyku_req:set([{buffer, <<>>}], Req))}.

-spec append_to_cowboyku_buffer(Buffer, Req) -> Req when
    Buffer :: iodata(),
    Req :: cowboyku_req:req().
append_to_cowboyku_buffer(Buffer, Req) ->
    [CowBuffer] = cowboyku_req:get([buffer], Req),
    cowboyku_req:set([{buffer, iolist_to_binary([CowBuffer, Buffer])}], Req).

%% Manually force a cowboyku request to be closed once the response is done
-spec mark_cowboyku_close(Req) -> Req when
    Req :: cowboyku_req:req().
mark_cowboyku_close(Req) ->
    cowboyku_req:set([{connection,close}], Req).

-spec mark_as_done(Req) -> Req when Req :: cowboyku_req:req().
mark_as_done(Req) ->
    self() ! {cowboyku_req, resp_sent},
    cowboyku_req:set([{resp_state, done}], Req).

% Config helpers
config(Key, Default) ->
    case application:get_env(?APP, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

config(Key) ->
    case application:get_env(?APP, Key) of
        undefined -> erlang:error({missing_config, Key});
        {ok, Val} -> Val
    end.

-spec get_via_value() -> binary().
get_via_value() ->
    <<"1.1 vegur">>.
