-module(vegur_validate_headers).
-behaviour(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    {Host, Req1} = cowboy_req:host(Req),
    case validate_host(Host, Req1, Env) of
        {ok, Req2, Env2} ->
            validate_content_length(Req2, Env2);
        Other ->
            Other
    end.

-spec validate_host(binary(), Req, Env) ->
                           {error, 400, Req} |
                           {ok, Req, Env} when
      Req :: cowboy_req:req(),
      Env :: cowboy_middleware:env().
validate_host(<<>>, Req, _Env) ->
    % The Host header is empty, return 400
    Req1 = vegur_utils:set_request_status(error, Req),
    {error, 400, Req1};
validate_host(_Host, Req, Env) ->
    {ok, Req, Env}.

%% We can't allow duplicate content-length headers with varying values,
%% or content-lengths where the values are separated by commas.
validate_content_length(Req, Env) ->
    {Headers, Req2} = cowboy_req:headers(Req),
    case validate_content_length1(Headers, undefined) of
        ok -> {ok, Req2, Env};
        error -> {error, 400, Req}
    end.

validate_content_length1([], _) -> ok;
validate_content_length1([{<<"content-length">>, Term} | Rest], Known) ->
    case binary:match(Term, <<",">>) of
        {_,_} ->
            error; % unsupported csv format
        nomatch ->
            case Known of
                undefined -> validate_content_length1(Rest, Term);
                Term -> validate_content_length1(Rest, Known);
                _Other -> error
            end
    end;
validate_content_length1([_ | Rest], Known) ->
    validate_content_length1(Rest, Known).
