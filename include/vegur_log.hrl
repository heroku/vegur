%%% Author : Geoff Cant <nem@erlang.geek.nz>
%%% Description : Logging macros
%%% Created : 13 Jan 2006 by Geoff Cant <nem@erlang.geek.nz>

-ifndef(logging_macros).
-define(logging_macros, true).

-define(LOG(Type, Fun, Req), vegur_request_log:log(Type, fun() ->
                                                                 Fun
                                                         end, Req)).

-endif. %logging
