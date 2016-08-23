%%% Copyright (c) 2013-2015, Heroku Inc <routing-feedback@heroku.com>.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are
%%% met:
%%%
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer.
%%%
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%%
%%% * The names of its contributors may not be used to endorse or promote
%%%   products derived from this software without specific prior written
%%%   permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
    %% We have a host, but we can't allow duplicates!
    {Headers, Req2} = cowboyku_req:headers(Req),
    case valid_host_count(Headers, 0) of
        ok ->
            {ok, Req2, Env};
        error ->
            {error, 400, Req}
    end.

%% more or less than 1 header field is bad
valid_host_count([], 1) -> ok;
valid_host_count([], _) -> error;
valid_host_count([{<<"host">>, _} | _], 1) -> error;
valid_host_count([{<<"host">>, _} | Rest], 0) -> valid_host_count(Rest, 1);
valid_host_count([_|T], N) -> valid_host_count(T, N).

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
