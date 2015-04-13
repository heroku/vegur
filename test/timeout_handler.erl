-module(timeout_handler).
-compile(export_all).

init({_Any, http}, Req, State) ->
    {ok, Req, State}.

handle(Req, [first]) ->
    timer:sleep(infinity),
    {ok, Req, [first]};
handle(Req, [other]) ->
    {{Trans, Sock}, Req2} = vegur_utils:raw_cowboyku_socket(Req),
    Trans:send(Sock, "HTTP/1.1 200 O"), % partial data
    timer:sleep(infinity),
    {ok, Req2, [other]}.

terminate(_Reason, _Req, _State) ->
    ok.

