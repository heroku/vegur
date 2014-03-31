-module(vegur_utils).

-define(APP, vegur).

-export([get_interface_module/1
         ,set_handler_state/2
         ,parse_header/2
         ,add_or_append_header/4
         ,add_if_missing_header/4
         ,add_or_replace_header/3
         ,delete_all_headers/2
         ,set_request_status/2
         ,get_request_status/1
         ,handle_error/2
         ,peer_ip_port/1
         ,raw_cowboy_socket/1
         ,raw_cowboy_sockbuf/1
        ]).

-export([config/1
         ,config/2]).

-spec get_interface_module(Req) ->
                                  {Module, HandlerState, Req}
                                      | no_return() when
      Req :: cowboy_req:req(),
      HandlerState :: term(),
      Module :: module().
get_interface_module(Req) ->
    {HandlerState, Req1} = cowboy_req:meta(handler_state, Req, undefined),
    {vegur_utils:config(interface_module), HandlerState, Req1}.

-spec set_handler_state(HandlerState, Req) -> Req when
      HandlerState :: term(),
      Req :: cowboy_req:req().
set_handler_state(HandlerState, Req) ->
    cowboy_req:set_meta(handler_state, HandlerState, Req).

-spec parse_header(binary(), cowboy_req:req()) ->
                          {ok, {[]|[binary()|undefined], cowboy_req:req()}}
                        | {error, badarg}.
parse_header(Key, Req) ->
    case cowboy_req:parse_header(Key, Req) of
        {ok, L, Req0} when is_list(L) -> {ok, {L, Req0}};
        {ok, Term, Req0} -> {ok, {[Term], Req0}};
        {undefined, Term, Req0} ->  {ok, {[Term], Req0}};
        {error, badarg} -> {error, badarg}
    end.

-spec add_or_append_header(Key, Value, Headers, Req) ->
                                  {Headers, Req} when
      Key :: iodata(),
      Value :: iodata(),
      Headers :: [{iodata(), iodata()}]|[],
      Req :: cowboy_req:req().
add_or_append_header(Key, Val, Headers, Req) ->
    case cowboy_req:header(Key, Req) of
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
      Req :: cowboy_req:req().
add_if_missing_header(Key, Val, Headers, Req) ->
    {NewVal, Req2} = 
        case cowboy_req:header(Key, Req) of
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

%% We need to traverse the entire list because a user could have
%% injected more than one instance of the same header, and cowboy
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
      Req :: cowboy_req:req().
set_response(Headers, Body, Req) ->
    Req1 = cowboy_req:set_resp_body(Body, Req),
    lists:foldl(fun({Name, Value}, R) ->
                        cowboy_req:set_resp_header(Name, Value, R)
                end, Req1, Headers).

-spec set_request_status(Status, Req) -> Req when
      Status :: vegur_interface:terminate_reason(),
      Req :: cowboy_req:req().
set_request_status(Status, Req) ->
    cowboy_req:set_meta(status, Status, Req).

-spec get_request_status(Req) -> {Status, Req} when
      Status :: vegur_interface:terminate_reason(),
      Req :: cowboy_req:req().
get_request_status(Req) ->
    cowboy_req:meta(status, Req).

-spec handle_error(Reason, Req) -> {HttpCode, Req} when
      Reason :: atom() | {Blame::atom(), term()},
      HttpCode :: cowboy:http_status(),
      Req :: cowboy_req:req().
handle_error(Reason, Req) ->
    {InterfaceModule, HandlerState, Req1} = get_interface_module(Req),
    {DomainGroup, Req2} = cowboy_req:meta(domain_group, Req1, undefined),
    {{HttpCode, ErrorHeaders, ErrorBody}, Req3, HandlerState1} = InterfaceModule:error_page(Reason, DomainGroup, Req2, HandlerState),
    Req4 = set_handler_state(HandlerState1, Req3),
    Req5 = set_response(ErrorHeaders, ErrorBody, Req4),
    Req6 = set_request_status(error, Req5),
    {HttpCode, Req6}.



-spec peer_ip_port(Req) -> {{IpAddress, PortNumber}|
                            {IpAddress, PortNumber, PortNumber}, Req} when
      IpAddress :: inet:ip_address(),
      PortNumber :: inet:port_number(),
      Req :: cowboy_req:req().
peer_ip_port(Req) ->
    Transport = cowboy_req:get(transport, Req),
    case Transport:name() of
        proxy_protocol_tcp ->
            ProxySocket = cowboy_req:get(socket, Req),
            {ok, {{PeerIp, PeerPort}, {_, DestPort}}} = Transport:proxyname(ProxySocket),
            {{PeerIp, PeerPort, DestPort}, Req};
        _ ->
            {{PeerIp, _}, Req3} = cowboy_req:peer(Req),
            {Port, Req4} = cowboy_req:port(Req3),
            {{PeerIp, Port}, Req4}
    end.

-spec raw_cowboy_socket(Req) ->  {{Transport, Socket}, Req} when
    Transport :: module(),
    Socket :: any(),
    Req :: cowboy_req:req().
raw_cowboy_socket(Req) ->
    [Transport, Socket] = cowboy_req:get([transport, socket], Req),
    {{Transport, Socket}, cowboy_req:set([{resp_state, done}], Req)}.

-spec raw_cowboy_sockbuf(Req) -> {{Transport, Socket}, Buffer, Req} when
    Transport :: module(),
    Socket :: any(),
    Buffer :: iodata(),
    Req :: cowboy_req:req().
raw_cowboy_sockbuf(Req) ->
    [Transport, Socket, Buffer] = cowboy_req:get([transport, socket, buffer], Req),
    {{Transport, Socket},
     Buffer,
     cowboy_req:set([{resp_state, done}, {buffer, <<>>}], Req)}.

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
