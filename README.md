# Vegur

Heroku's proxy library based on a forked Cowboy frontend. This libary handles
proxying in Heroku's routing stack

![Illfær vegur](http://i.imgur.com/lwRxWDz.jpg)

And how do you pronounce vegur? Like [this](https://soundcloud.com/omarkj/vegur).

## Build

    $ rebar get-deps compile

## Test

    $ rebar ct

## Writing a Router

Vegur is a *proxy* application, meaning that it takes care of receiving HTTP
requests and forwarding them to another server; similarly for responses.

What it isn't is a *router*, meaning that it will not handle choosing which
nodes to send traffic to, nor will it actually track what backends are
available. This task is left to the user of the library, by writing a router
callback module.

`src/vegur_stub.erl`, which provides an example implementation of the callback
module that has to be used to implement routing logic, can be used as a source
of information.


### Demo reverse-proxy

To set up a reverse-proxy that does load balancing locally, we'll first set up
two toy servers:

```bash
$ while true ; do  echo -e "HTTP/1.1 200 OK\r\nConnection:close\r\nContent-Length: ${#$(date)}\r\n\r\n$(date)" | nc -l -p 8081 ; done
$ while true ; do  echo -e "HTTP/1.1 200 OK\r\nConnection:close\r\nContent-Length: ${#$(date)}\r\n\r\n$(date)" | nc -l -p 8082 ; done
```

These have the same behaviour and will do the exact same thing, except one is
on port 8081 and the other is on port 8082. You can try reaching them from your
browser.

To make things simple, I'm going to hardcode both back-ends directly in the
source module:

```erlang
-module(toy_router).
-behaviour(vegur_interface).
-export([init/2,
         terminate/3,
         lookup_domain_name/3,
         checkout_service/3,
         checkin_service/6,
         service_backend/3,
         feature/2,
         additional_headers/2,
         error_page/4]).

-record(state, {tries = [] :: list()}).
```

This is our list of exported functions, along with the behaviour they implement
(`vegur_interface`), and a record defining the internal state of each router
invocation. We track a single value, `tries`, which will be useful to
make sure we don't end up in an infinite loop if we ever have no backends
alive.

Now for the implementation of specific callbacks, documented in
`src/vegur_stub.erl`:

```erlang
init(AcceptTime, Upstream) ->
    random:seed(AcceptTime), % RNGs require per-process seeding
    {ok, Upstream, #state{}}. % state initialization here.

lookup_domain_name(_ReqDomain, _Upstream, State) ->
    %% hardcoded values, we don't care for the domain
    Servers = [{1, {127,0,0,1}, 8081},
               {2, {127,0,0,1}, 8082}],
    {ok, Servers, State}.
```

From there on, we then can fill in the checkin/checkout logic. We technically
have a limitation of one request at a time per server, but we won't track
these limitations outside of a limited number of connection retries.

```erlang
checkout_service(Servers, Upstream, State=#state{tries=Tried}) ->
    Available = Servers -- Tried,
    case Available of
        [] ->
            {error, all_blocked, Upstream, State};
        _ ->
            N = random:uniform(length(Available)),
            Pick = lists:nth(N, Available),
            {service, Pick, State#state{tries=[Pick | Tried]}}
    end.

service_backend({_Id, IP, Port}, Upstream, State) ->
    %% extract the IP:PORT from the chosen server.
    {{IP, Port}, Upstream, State}.

checkin_service(_Servers, _Pick, _Phase, _ServState, Upstream, State) ->
    %% if we tracked total connections, it'd be here we decrement the counters
    {ok, Upstream, State}.
```

We're also going to enable none of the features and add no headers because
this is a basic demo:

```erlang
feature(_WhoCares, State) ->
    {disabled, State}.

additional_headers(_Log, State) ->
    {[], State}.
```

And error pages. For now we only care about the one we return, which is `all_blocked`:

```erlang
error_page(all_blocked, _DomainGroup, Upstream, State) ->
    {{502, [], <<>>}, Upstream, State}; % Bad Gateway
```

And then all the default ones, which I copy from the `vegur_stub` module:

```erlang
%% Vegur-returned errors that should be handled no matter what
error_page(expectation_failed, _DomainGroup, Upstream, HandlerState) ->
    {{417, [], <<>>}, Upstream, HandlerState};
error_page({upstream, closed}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, closed}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, timeout}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({upstream, timeout}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({undefined, timeout}, _DomainGroup, Upstream, HandlerState) ->
    %% Who knows who timed out. Technically both!
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, invalid_status}, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState};
error_page({downstream, content_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, cookie_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, header_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, status_length}, _DomainGroup, Upstream, HandlerState) ->
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({upstream, {bad_chunk,_}}, _DomainGroup, Upstream, HandlerState) ->
    %% bad chunked encoding from client
    {{400, [], <<>>}, Upstream, HandlerState};
error_page({upstream, invalid_transfer_encoding}, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page({downstream, {bad_chunk,_}}, _DomainGroup, Upstream, HandlerState) ->
    %% bad chunked encoding from server
    {{502, [], <<>>}, Upstream, HandlerState};
error_page({downstream, non_terminal_status_after_continue}, _DomainGroup, Upstream, HandlerState) ->
    %% Can't send a 1xx status after a 100 continue (except for upgrades)
    %% when expect: 100-continue is declared
    {{502, [], <<>>}, Upstream, HandlerState};
%% Generic handling
error_page(empty_host, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request_header, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(_, _DomainGroup, Upstream, HandlerState) ->
    {{503, [], <<>>}, Upstream, HandlerState}.
```

And then terminate in a way we don't care:

```erlang
terminate(_, _, _) ->
    ok.
```

And then we're done. Compile all that stuff:

```bash
$ rebar compile && erl -pa ebin demo deps/*/ebin
Erlang/OTP 17 [erts-6.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.0  (abort with ^G)
1> c("demo/toy_router"), application:ensure_all_started(vegur), vegur:start_http(8080, toy_router, [{middlewares, vegur:default_middlewares()}]).
{ok,<0.62.0>}
```

You can then call localhost:8080 and see the request routed to either of your
netcat servers.

Congratulations, you have a working reverse-load balancer and/or proxy/router
combo running. You can shut down either server, the other should take the load,
or get an error once nothing is left available.

## Behaviour

There are multiple specific HTTP behaviours that have been chosen/implemented
in this proxying software. The list is maintained at
https://devcenter.heroku.com/articles/heroku-improved-router

## Configuration

### OTP Configuration

The configuration can be passed following the standard Erlang/OTP application logic.

- `{acceptors, pos_integer()}`: number of HTTP acceptors expected. Defaults to
  `1024`.
- `{max_connections, pos_integer()}`: max number of active HTTP connections
  (inbound). Defaults to `100000`.
- `{request_id_name, binary()}`: Vegur will read a request id header and pass
  it on to the proxied request. It will also automatically insert a header with
  a request id if none is present. This item configures the name of such an ID,
  and defaults to `X-Request-Id`.
- `{request_id_max_size, pos_integer()}`: The request Id submitted can be
  forced to have a maximal size, after which it is considered invalid and a new
  one is generated. Defaults to `200`.
- `{start_time_header, binary()}`: Vegur will insert a header representing the
  epoch at which the request started based on the current node's clock. This
  allows to specify the name of that header. Defaults to `X-Request-Start`.
- `{connect_time_header, binary()}`: A header is added noting the time it took
  to establish a connection to the back-end node provided. This allows to set
  the name of this header. Defaults to `Connect-Time`.
- `{route_time_header, binary()}`: A header is added noting the time it took
  the routing callback module to make its decision. This allows to set the name
  of this header. Defaults to `Total-Route-Time`.
- `{idle_timeout, non_neg_integer()}`: Maximal period of inactivity during a
  session, in seconds. Defaults to 55.
- `{downstream_connect_timeout, timeout()}`: Maximal time period to wait before
  abandoning the connection to a backend, in milliseconds. Defaults to 5000ms.
- `{downstream_timeout, non_neg_integer()}`: Maximal time period to wait before
  abandonning the wait for a response after a request has been forwarded to a
  back-end, in seconds. Defaults to 30. This value is purely for the initial
  response, after which `idle_timeout` takes over as a value.
- `{client_tcp_buffer_limit, pos_integer()}`: Size of the TCP buffer for the
  socket to the backend server, in bytes. Defaults to `1048576` (`1024*1024`
  bytes).
- `{max_client_status_length, pos_integer()}`: Maximal size of the status line
  of the client response, in bytes. Defaults to `8192`.
- `{max_client_header_length, pos_integer()}`: Maximal size of a given response
  header line, in bytes. Defaults to `524288`, or 512kb.
- `{max_client_cookie_length, pos_integer()}`: Maximal size of a cookie in a
  response, in bytes. Defaults to `8192`.

### Server Configuration

The HTTP servers themselves can also have their own configuration in a
per-listener manner. The following options are valid when passed to
`vegur:start/5`:

- `{max_request_line_length, pos_integer()}`: Maximal line size for the HTTP
  request. Defaults to 8192. Note that this value may be disregarded if the
  entire line managed to fit within the confines of a single HTTP packet or
  `recv` operation.
- `{max_header_name_length, pos_integer()}`: Maximal length for header names in
  HTTP requests. Defaults to `1000`. Note that this value may be disregarded if
  the entire line managed to fit within the confines of a single HTTP packet or
  `recv` operation.
- `{max_header_value_length, pos_integer()}`: Maximal length for the value of a
  header in HTTP requests. Defaults to `8192`. Note that this value may be
  disregarded if the entire line managed to fit within the confines of a single
  HTTP packet or `recv` operation.
- `{max_headers, pos_integer()}`: number of HTTP headers allowed in a single
  request. Defaults to 1000.
- `{timeout, timeout()}`: Delay, in milliseconds, after which a connection is
  closed for inactivity. This delay also specifies the maximal time that an
  idle connection being pre-opened by some service for efficiency reasons will
  remain open without receiving a request on it.

It is recommended that options regarding header sizes for the HTTP listener
match the options for the `max_cookie_length` in the OTP options to avoid the
painful case of a backend setting a cookie that cannot be sent back by the end
client.

Logs and statistics being collected
-----------------------------------

* `domain_lookup`
 * Time it takes to lookup the domain in the domain service.
* `service_lookup`
 * Time it takes to lookup a service to connect to.
* `connect_time`
 * Time it takes to connect to the backend server.
*  `pre_connect`
 * Timestamp before connecting to the backend server
* `connection_accepted`
 * Timestamp when connection is accepted

Reference Material
------------------

* [HTTP Made Easy](http://www.jmarshall.com/easy/http/)
* [HTTPbis Wiki](http://trac.tools.ietf.org/wg/httpbis/trac/wiki)
* [HTTPbis RFC - Messaging](http://tools.ietf.org/html/draft-ietf-httpbis-p1-messaging-24)
* [HTTPbis RFC - Semantics](http://tools.ietf.org/html/draft-ietf-httpbis-p2-semantics-24)
* [The Cowboy Guide](http://ninenines.eu/docs/en/cowboy/HEAD/guide/introduction)
* [Key differences between HTTP/1.0 and 1.1](http://www8.org/w8-papers/5c-protocols/key/key.html)
