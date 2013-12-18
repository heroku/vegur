hstub
=====

A testing ground for new proxy implementations based on a cowboy frontend.

Build
-----

    $ rebar get-deps compile

Run
---

    $ foreman start web

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

Try it out
----------

    $ curl http://localhost:9880

Reference Material
------------------

* [HTTP Made Easy](http://www.jmarshall.com/easy/http/)
* [HTTPbis Wiki](http://trac.tools.ietf.org/wg/httpbis/trac/wiki)
* [HTTPbis RFC - Messaging](http://tools.ietf.org/html/draft-ietf-httpbis-p1-messaging-24)
* [HTTPbis RFC - Semantics](http://tools.ietf.org/html/draft-ietf-httpbis-p2-semantics-24)
* [The Cowboy Guide](http://ninenines.eu/docs/en/cowboy/HEAD/guide/introduction)
* [Key differences between HTTP/1.0 and 1.1](http://www8.org/w8-papers/5c-protocols/key/key.html)
