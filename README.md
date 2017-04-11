# Vegur

[![Build Status](https://travis-ci.org/heroku/vegur.svg?branch=master)](https://travis-ci.org/heroku/vegur)

Heroku's proxy library based on a forked Cowboy frontend (Cowboyku). This
library handles proxying in Heroku's routing stack

![Illfær vegur](http://i.imgur.com/lwRxWDz.jpg)

And how do you pronounce vegur? Like [this](https://soundcloud.com/omarkj/vegur).

## Build

    $ rebar3 compile

## Test

    $ rebar3 ct

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
$ while true; do ( BODY=$(date); echo -e "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: ${#BODY}\r\n\r\n$BODY" | nc -l -p 8081 ); done
$ while true; do ( BODY=$(date); echo -e "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Length: ${#BODY}\r\n\r\n$BODY" | nc -l -p 8082 ); done
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
         additional_headers/4,
         error_page/4]).

-record(state, {tries = [] :: list()}).
```

This is our list of exported functions, along with the behaviour they implement
(`vegur_interface`), and a record defining the internal state of each router
invocation. We track a single value, `tries`, which will be useful to
make sure we don't end up in an infinite loop if we ever have no backends
alive.

An important thing to note is that this `toy_router` module will be called once
per request and is decentralized with nothing shared, unlike a node-unique
`gen_server`.

Now for the implementation of specific callbacks, documented in
`src/vegur_stub.erl`:

```erlang
init(_AcceptTime, Upstream) ->
    {ok, Upstream, #state{}}. % state initialization here.

lookup_domain_name(_ReqDomain, Upstream, State) ->
    %% hardcoded values, we don't care about the domain
    Servers = [{1, {127,0,0,1}, 8081},
               {2, {127,0,0,1}, 8082}],
    {ok, Servers, Upstream, State}.
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
            N = rand:uniform(length(Available)),
            Pick = lists:nth(N, Available),
            {service, Pick, Upstream, State#state{tries=[Pick | Tried]}}
    end.

service_backend({_Id, IP, Port}, Upstream, State) ->
    %% Extract the IP:PORT from the chosen server.
    %% To enable keep-alive, use:
    %% `{{keepalive, {default, {IP,Port}}}, Upstream, State}'
    %% To force the use of a new keepalive connection, use:
    %% `{{keepalive, {new, {IP,Port}}}, Upstream, State}'
    %% Otherwise, no keepalive is done to the back-end:
    {{IP, Port}, Upstream, State}.

checkin_service(_Servers, _Pick, _Phase, _ServState, Upstream, State) ->
    %% if we tracked total connections, we would decrement the counters here
    {ok, Upstream, State}.
```

We're also going to enable none of the features and add no headers in either
direction because this is a basic demo:

```erlang
feature(_WhoCares, State) ->
    {disabled, State}.

additional_headers(_Direction, _Log, _Upstream, State) ->
    {[], State}.
```

And error pages. For now we only care about the one we return, which is `all_blocked`:

```erlang
error_page(all_blocked, _DomainGroup, Upstream, State) ->
    {{502, [], <<>>}, Upstream, State}; % Bad Gateway
```

And then the default ones, which I define broadly:

```erlang
%% Vegur-returned errors that should be handled no matter what.
%% Full list in src/vegur_stub.erl
error_page({upstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
    %% Blame the caller
    {{400, [], <<>>}, Upstream, HandlerState};
error_page({downstream, _Reason}, _DomainGroup, Upstream, HandlerState) ->
    %% Blame the server
    {{500, [], <<>>}, Upstream, HandlerState};
error_page({undefined, _Reason}, _DomainGroup, Upstream, HandlerState) ->
    %% Who knows who was to blame!
    {{500, [], <<>>}, Upstream, HandlerState};
%% Specific error codes from middleware
error_page(empty_host, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(bad_request, _DomainGroup, Upstream, HandlerState) ->
    {{400, [], <<>>}, Upstream, HandlerState};
error_page(expectation_failed, _DomainGroup, Upstream, HandlerState) ->
    {{417, [], <<>>}, Upstream, HandlerState};
%% Catch-all
error_page(_, _DomainGroup, Upstream, HandlerState) ->
    {{500, [], <<>>}, Upstream, HandlerState}.
```

And then terminate without doing anything special (we don't have state
to tear down, for example):

```erlang
terminate(_, _, _) ->
    ok.
```

And then we're done. Compile all that stuff:

```bash
$ rebar3 shell
Erlang/OTP 17 [erts-6.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.0  (abort with ^G)
1> c("demo/toy_router"), application:ensure_all_started(vegur), vegur:start_http(8080, toy_router, [{middlewares, vegur:default_middlewares()}]).
{ok,<0.62.0>}
```

You can then call localhost:8080 and see the request routed to either of your
netcat servers.

Congratulations, you have a working reverse-load balancer and/or proxy/router
combo running. You can shut down either server. The other should take the load,
and if it also fails, the user would get an error since nothing is left
available.

## Behaviour

There are multiple specific HTTP behaviours that have been chosen/implemented
in this proxying software. The list is maintained at
https://devcenter.heroku.com/articles/http-routing

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
- `{extra_socket_options, [gen_tcp:option()]}`: Allows to set additional
  TCP options useful for configuration (such as `nodelay` or `raw` options).

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

## Middlewares ##

Vegur supports a middleware interface that can be configured when booting
the application. These can be configured by setting the `middlewares` option:

```erlang
vegur:start_http(Port, CallbackMod, [{middlewares, Middlewares}]),
vegur:start_proxy(Port, CallbackMod, [{middlewares, Middlewares}]),
```

The middlewares value should always contain, at the very least, the result of
`vegur:default_middlewares()`, which implements some required functionality.

For example, the following middlewares are the default ones:

- `vegur_validate_headers`: ensures the presence of `Host` headers, and that
  `content-length` headers are legitimate without duplication;
- `vegur_lookup_domain_middleware`: calls the callback module to do domain
  lookups and keeps it in state;
- `vegur_continue_middleware`: handles `expect: 100-continue` headers
  conditionally depending on the `feature` configured by the callback
  module;
- `vegur_upgrade_middleware`: detects if the request needs an `upgrade`
  (for example, websockets) and sets internal state for the proxy to
  properly handle this once it negotiates headers with the back-end;
- `vegur_lookup_service_middleware`: calls the callback module to pick
  a back-end for the current domain;
- `vegur_proxy_middleware`: actually proxies the request

The order is important, and as defined, default middlewares *must* be
kept for a lot of functionality (from safety to actual proxying) to
actually work.

Custom middlewares can still be added throughout the chain by adding
them to the list.

### Defining middlewares

The middlewares included are standard `cowboyku` (`cowboy` ~0.9)
middlewares and respect the same interface.

There's a single callback defined:

```erlang
execute(Req, Env)
    -> {ok, Req, Env}
     | {suspend, module(), atom(), [any()]}
     | {halt, Req}
     | {error, cowboyku:http_status(), Req}
    when Req::cowboyku_req:req(), Env::env().
```

For example, a middleware implementing some custom form of
authentication where a secret token is required to access
data could be devised to work like:

```erlang
module(validate_custom_auth).
-behaviour(cowboyku_middleware).
-export([execute/2]).

-define(TOKEN, <<"abcdef">>. % this is really unsafe

execute(Req, Env) ->
    case cowboyku_req:header(<<"my-token">>, Req) of
        {?TOKEN, Req2} ->
            {ok, Req2, Env};
        {_, Req2} ->
            {HTTPCode, Req3} = vegur_utils:handle_error(bad_token, Req2),
            {error, HTTPCode, Req3}
    end.
```

Calling `vegur_utils:handle_error(Reason, Req)` will redirect
the error to the `Callback:error_page/4` callback, letting the
custom callback module set its own HTTP status, handle logging,
and do whatever processing it needs before stopping the request.


## Logs and statistics being collected

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



## Behaviour

### Added Headers

All headers are considered to be case-insensitive, as per the HTTP
Specification, but will be camel-cased by default. A few of them are added by
Vegur.

- `X-Forwarded-For`: the originating IP address of the client connecting to the
  proxy
- `X-Forwarded-Proto`: the originating protocol of the HTTP request (example:
  https). This is detected based on the incoming port, so using port 8080 will
  not add this header.
- `X-Forwarded-Port`: the originating port of the HTTP request (example: 443)
- `X-Request-Start`: unix timestamp (milliseconds) when the request was
  received by the proxy
- `X-Request-Id`: the HTTP Request ID
- `Via`: a code name for the vegur proxy, with the value `vegur: 1.1`
- `Server`: will be added to the response (using our forked `cowboy`) if the
  endpoint didn't add it first.

### Protocol Details

The vegur proxy only supports HTTP/1.0 and HTTP/1.1 clients. HTTP/0.9 and
earlier are no longer supported. SPDY and HTTP/2.0 are not supported at this
point.

The proxy's behavior is to be as compliant as possible with the HTTP/1.1
specifications. Special exceptions must be made for HTTP/1.0 however:

- The proxy will advertise itself as using HTTP/1.1 regardless whether the
  client uses HTTP/1.0 or not.
- It is the proxy's responsibility to convert a chunked response to a regular
  HTTP response. In order to do so without accumulating potentially gigabytes
  of data, the response to the client will be delimited by the termination
  of the connection
  (See [Point 4.4.5](http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.4))
- The router will assume that the client wants to close the connection on each
  request (no keep-alive).
- An HTTP/1.0 client may send a request with an explicit `connection:keep-alive`
  header. Despite the keep-alive mechanism not being defined back in 1.0 (it
  was ad-hoc), the router makes the assumption that the behavior requested is
  similar to the HTTP/1.1 behavior at this point.

Other details:

- No caching done by the proxy
- Websockets (and the general `upgrade` mechanism) are supported
- Responses are not compressed on behalf of the application
- All HTTP methods are supported, except `CONNECT`.
- `Expect: 100-continue` requests can be automatically answered to with `100
  Continue` or forwarded to the application based on the `feature` routing
  callback function.
- Only `100-continue` is accepted as a value for `expect` headers. In case any
  other value is encountered, the proxy responds with `417 Expectation Failed`
- The proxy will ignore `Connection: close` on a `100 Continue` and only honor
  it after it receives the final response. Note however, that because
  `Connection: close` is a hop-by-hop mechanism, the proxy will not necessarily
  close the connection to the client, and may not forward it.
- By default, the proxy will close all connections to the back-ends after each
  request, but will honor keep-alive to the client when possible. Support
  for keep-alive to the back-end can be enabled by returning the right values
  out of the `service_backend` callback.
- The proxy will return a configurable error code if the server returns a `100
  Continue` following an initial `100 Continue` response. The proxy does not
  yet support infinite `1xx` streams.
- In the case of chunked encoding and `content-length` both being present in
  the request, the router will [give precedence to chunked
  encoding](http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.4).
- If multiple `content-length` fields are present, and that they have the same
  length, they will be merged into a single `content-length` header
- If a `content-length` header contains multiple values (`content-length: 15,24`)
  or a request contains multiple content-length headers with multiple values,
  the request will be denied with a code `400`.
- Headers are restricted to 8192 bytes per line (and 1000 bytes for the header
  name)
- Hop-by-hop headers will be stripped to avoid confusion
- At most, 1000 headers are allowed per request
- The request line of the HTTP request is limited to 8192 bytes

Specifically for responses:


- Hop-by-hop headers will be stripped to avoid confusion
- Headers are restricted to 512kb per line
- Cookies are explicitly restricted to 8192 bytes. This is to protect against
  common restrictions (for example, imposed by CDNs) that rarely accept larger
  cookie values. In such cases, a developer could accidentally set large cookies,
  which would be submitted back to the user, who would then see all of his or her
  requests denied.
- The status line (`HTTP/1.1 200 OK`) is restricted to 8192 bytes in length,
  must have a 3-digit response code and contain a string explaining the code,
  as per RFC.

Additionally, while HTTP/1.1 requests and responses are expected to be
keep-alive by default, if the initial request had an explicit `connection: close`
header from the router to the backend, the backend can send a response
delimited by the connection termination, without a specific content-encoding
nor an explicit content-length.

Even though the `HEAD` HTTP verb does not require a response body to be
sent over the line and ends at the response headers, `HEAD` requests are
explicitly made to work with `101 Switching Protocols` responses. A backend that
doesn't want to upgrade should send a different status code, and the connection
will not be upgraded.

## Not Supported

- SPDY
- HTTP/2.x
- `Expect` headers with any content other than 100-continue (yields a `417`)
- HTTP Extensions such as WEBDAV, relying on additional 1xx status responses
- A HEAD, 1xx, 204, or 304 response which specifies a content-length or chunked
  encoding will result in the proxy forwarding such headers, but not the body
  that may or may not be coming with the response.
- Header line endings other than CRLF (`\r\n`)
- Caching of HTTP Content
- Caching the HTTP versions of backends
- Long-standing preallocated idle connections. The limit is set to 1 minute
  before an idle connection is closed.
- HTTP/1.0 routing without a `Host` header, even when the full path is
  submitted in the request line.

## Contributing

All contributed work must have:

- Tests
- Documentation
- Rationale
- Proper commit description.

A good commit message should include a rationale for the change, along with the
existing, expected, and new behaviour.

All contributed work will be reviewed before being merged (or rejected).

This proxy is used in production with existing apps, and a commitment to
backwards compatibility (or just working in the real world) is in place.

## Architecture Guidelines

Most of the request validation is done through the usage of middlewares.
The middlewares we use are implemented through `midjan`, which wraps some
operations traditionally done by `cowboyku` in order to have more control
over vital parts of a request/response whenever the RFC is different
between servers and proxies.

All middleware modules have their name terminated by `_middleware`.

The proxy is then split into 5 major parts maintained in this directory:

1. `vegur_proxy_middleware`, which handles the high-level request/response
   patterns.
2. `vegur_proxy`, which handles the low-level HTTP coordination between
   requests and responses, and technicalities of socket management, header
   reconciliation, etc.
3. `vegur_client`, a small HTTP client to call back-ends
4. Supporting sub-states of HTTP, such as the chunked parser and the bytepipe
   (used for upgrades), each having its own module (`vegur_chunked` and
   `vegur_bytepipe`)
5. Supporting modules, such as functional logging modules, midjan translators,
   and so on (`vegur_req_log`, `vegur_midjan_translator`).


## Reference Material

* [HTTP Made Easy](http://www.jmarshall.com/easy/http/)
* [HTTP/1.0 RFC](http://www.isi.edu/in-notes/rfc1945.txt)
* [HTTP/1.1 RFC (original)](https://www.ietf.org/rfc/rfc2068.txt)
* [HTTP/1.1 RFC (updated)](https://www.ietf.org/rfc/rfc2616.txt)
* [HTTP/1.1 RFCs (updated again, by HTTPbis)](https://datatracker.ietf.org/wg/httpbis/documents/),
  particularly the [Messaging RFC](https://datatracker.ietf.org/doc/rfc7230/), and
  the [Semantics RFC](https://datatracker.ietf.org/doc/rfc7231/)
* [HTTPbis Wiki](http://trac.tools.ietf.org/wg/httpbis/trac/wiki)
* [The Cowboy Guide](http://ninenines.eu/docs/en/cowboy/HEAD/guide/introduction)
* [Key differences between HTTP/1.0 and 1.1](http://www8.org/w8-papers/5c-protocols/key/key.html)
* [What Proxies Must Do](https://www.mnot.net/blog/2011/07/11/what_proxies_must_do)
* [Heroku's HTTP Routing](https://devcenter.heroku.com/articles/http-routing)

## Changelog

- 2.0.5: `Expect` header can be empty
- 2.0.4: vegur_client returns error on invalid encoding types
- 2.0.3: reinstate `X-Forwarded-Host` as too much stuff breaks without it
- 2.0.2: drop duplicate `Host` headers and `X-Forwarded-Host` for cache issues
- 2.0.1: enable `SO_REUSEADDR` on connections to backend to support more connections
- 2.0.0: adding support for keepalive to the backend, dropping support for OTP 16 and 17
- 1.1.1: minor refactoring, typespecs and documentation changes
- 1.1.0: initial support for PROXY protocol v2
- 1.0.0: first stable release
