-module(vegur_validate_headers).
-behaviour(cowboyku_middleware).
-export([execute/2]).

execute(Req, Env) ->
    {Host, Req1} = cowboyku_req:host(Req),
    case validate_host(Host, Req1, Env) of
        {ok, Req2, Env2} ->
            validate_content_length(Req2, Env2);
        Other ->
            Other
    end.

-spec validate_host(binary(), Req, Env) ->
                           {error, 400, Req} |
                           {ok, Req, Env} when
      Req :: cowboyku_req:req(),
      Env :: cowboyku_middleware:env().
validate_host(<<>>, Req, _Env) ->
    % The Host header is empty, return 400
    {HttpCode, Req1} = vegur_utils:handle_error(empty_host, Req),
    {error, HttpCode, Req1};
validate_host(_Host, Req, Env) ->
    {ok, Req, Env}.

%% We can't allow duplicate content-length headers with varying values,
%% or content-lengths where the values are separated by commas.
validate_content_length(Req, Env) ->
    {Headers, Req2} = cowboyku_req:headers(Req),
    case validate_content_length1(Headers, undefined) of
        ok ->
            {ok, Req2, Env};
        error ->
            %% @todo add error handling
            {error, 400, Req}
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
