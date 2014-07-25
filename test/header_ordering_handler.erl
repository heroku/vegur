-module(header_ordering_handler).
-compile(export_all).

init({_Any, http}, Req, _) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200,
                                  [{<<"test-header">>, <<"1">>},
                                   {<<"test-header">>, <<"2">>},
                                   {<<"test-header">>, <<"3">>}],
                                  <<"OK">>,
                                  Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
