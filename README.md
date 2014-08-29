# Vegur

Heroku's proxy library based on a Cowboy frontend. This libary handles
proxying in Heroku's new keep-alive routing stack

![Illfær vegur](http://i.imgur.com/lwRxWDz.jpg)

And how do you pronounce vegur? Like [this](https://soundcloud.com/omarkj/vegur).

## Build

    $ rebar get-deps compile

## Test

    $ rebar ct

## Writing a Router

Vegur is a *proxy* application, meaning that it takes care of receiving HTTP requests and
forwarding them to another server; similarly for responses.

What it isn't is a *router*, meaning that it will not handle choosing which nodes to send
traffic to, nor will it actually track what backends are available. This task is left to
the user of the library, by writing a router callback module.

The documentation for this isn't complete yet. In the mean time, feel free to reverse-engineer
what has been done in `src/vegur_stub.erl`, which provides an example implementation.

## Behaviour

There are multiple specific HTTP behaviours that have been chosen/implemented in this proxying
software. The list is maintained at https://devcenter.heroku.com/articles/heroku-improved-router

## Configuration

### OTP Configuration

The configuration can be passed following the standard Erlang/OTP application logic.

- `{acceptors, pos_integer()}`: number of HTTP acceptors expected. Defaults to `1024`.
- `{max_connections, pos_integer()}`: max number of active HTTP connections (inbound). Defaults to `100000`.
- `{request_id_name, binary()}`: Vegur will read a request id header and pass it on to the proxied request. It will also automatically insert a header with a request id if none is present. This item configures the name of such an ID, and defaults to `X-Request-Id`.
- `{request_id_max_size, pos_integer()}`: The request Id submitted can be forced to have a maximal size, after which it is considered invalid and a new one is generated. Defaults to `200`.
- `{start_time_header, binary()}`: Vegur will insert a header representing the epoch at which the request started based on the current node's clock. This allows to specify the name of that header. Defaults to `X-Request-Start`.
- `{connect_time_header, binary()}`: A header is added noting the time it took to establish a connection to the back-end node provided. This allows to set the name of this header. Defaults to `Connect-Time`.
- `{route_time_header, binary()}`: A header is added noting the time it took the routing callback module to make its decision. This allows to set the name of this header. Defaults to `Total-Route-Time`.
- `{idle_timeout, non_neg_integer()}`: Maximal period of inactivity during a session, in seconds. Defaults to 55.
- `{downstream_connect_timeout, timeout()}`: Maximal time period to wait before abandoning the connection to a backend, in milliseconds. Defaults to 5000ms.
- `{downstream_timeout, non_neg_integer()}`: Maximal time period to wait before abandonning the wait for a response after a request has been forwarded to a back-end, in seconds. Defaults to 30. This value is purely for the initial response, after which `idle_timeout` takes over as a value.
- `{client_tcp_buffer_limit, pos_integer()}`: Size of the TCP buffer for the socket to the backend server, in bytes. Defaults to `1048576` (`1024*1024` bytes).
- `{max_client_status_length, pos_integer()}`: Maximal size of the status line of the client response, in bytes. Defaults to `8192`.
- `{max_client_header_length, pos_integer()}`: Maximal size of a given response header line, in bytes. Defaults to `524288`, or 512kb.
- `{max_client_cookie_length, pos_integer()}`: Maximal size of a cookie in a response, in bytes. Defaults to `8192`.

### Server Configuration

The HTTP servers themselves can also have their own configuration in a per-listener manner. The following options are valid when passed to `vegur:start/5`:

- `{max_request_line_length, pos_integer()}`: Maximal line size for the HTTP request. Defaults to 8192. Note that this value may be disregarded if the entire line managed to fit within the confines of a single HTTP packet or `recv` operation.
- `{max_header_name_length, pos_integer()}`: Maximal length for header names in HTTP requests. Defaults to `1000`. Note that this value may be disregarded if the entire line managed to fit within the confines of a single HTTP packet or `recv` operation.
- `{max_header_value_length, pos_integer()}`: Maximal length for the value of a header in HTTP requests. Defaults to `8192`. Note that this value may be disregarded if the entire line managed to fit within the confines of a single HTTP packet or `recv` operation.
- `{max__headers, pos_integer()}`: number of HTTP headers allowed in a single request. Defaults to 1000.
- `{timeout, timeout()}`: Delay, in milliseconds, after which a connection is closed for inactivity. This delay also specifies the maximal time that an idle connection being pre-opened by some service for efficiency reasons will remain open without receiving a request on it.

It is recommended that options regarding header sizes for the HTTP listener match the options for the `max_cookie_length` in the OTP options to avoid the painful case of a backend setting a cookie that cannot be sent back by the end client.

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
