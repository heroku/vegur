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
-module(vegur_headers).

-define(APP, vegur).

%% High-level interface
-export([request_headers/2,
         request_headers/3,
         response_headers/1,
         upgrade_response_headers/1]).

%% Individual Methods
-export([delete_host_header/1,
         delete_content_length_header/1,
         delete_transfer_encoding_header/1,
         delete_hop_by_hop/1,

         add_connection_close_header/1,
         add_connection_keepalive_header/1,
         add_connection_upgrade_header/1,
         add_via/1]).

%% Strip Connection header on request.
request_headers(Headers0, Type) ->
    request_headers(Headers0, Type, false).

request_headers(Headers0, Type, ShouldKeepalive) ->
    HeaderFuns = case Type of
        [upgrade] ->
            [fun delete_host_header/1
            ,fun delete_hop_by_hop/1
            ,fun add_connection_upgrade_header/1
            ,fun delete_content_length_header/1];
        _ when ShouldKeepalive ->
            [fun delete_host_header/1
            ,fun delete_hop_by_hop/1
            ,fun add_connection_keepalive_header/1
            ,fun delete_content_length_header/1];
        _ ->
            [fun delete_host_header/1
            ,fun delete_hop_by_hop/1
            ,fun add_connection_close_header/1
            ,fun delete_content_length_header/1]
    end,
    lists:foldl(fun (F, H) -> F(H) end, Headers0, HeaderFuns).

%% Strip Hop-by-hop headers on a response that is being
%% upgraded
upgrade_response_headers(Headers) ->
    lists:foldl(fun (F, H) -> F(H) end,
                Headers,
                [fun delete_hop_by_hop/1,
                 fun add_connection_upgrade_header/1,
                 fun add_via/1
                ]).

%% Strip Hop-by-hop headers on response
response_headers(Headers) ->
    lists:foldl(fun (F, H) -> F(H) end,
                Headers,
                [fun delete_hop_by_hop/1,
                 fun add_via/1
                ]).

delete_host_header(Hdrs) ->
    vegur_utils:delete_all_headers(<<"host">>, Hdrs).

delete_content_length_header(Hdrs) ->
    vegur_utils:delete_all_headers(<<"content-length">>, Hdrs).

delete_transfer_encoding_header(Hdrs) ->
    vegur_utils:delete_all_headers(<<"transfer-encoding">>, Hdrs).

%% Hop by Hop Headers we care about removing. We remove most of them but
%% "Proxy-Authentication" for historical reasons, "Upgrade" because we pass
%% it through, "Transfer-Encoding" because we restrict to 'chunked' and pass
%% it through.
delete_hop_by_hop([]) -> [];
delete_hop_by_hop([{<<"connection">>, _} | Hdrs]) -> delete_hop_by_hop(Hdrs);
delete_hop_by_hop([{<<"te">>, _} | Hdrs]) -> delete_hop_by_hop(Hdrs);
delete_hop_by_hop([{<<"keep-alive">>, _} | Hdrs]) -> delete_hop_by_hop(Hdrs);
delete_hop_by_hop([{<<"proxy-authorization">>, _} | Hdrs]) -> delete_hop_by_hop(Hdrs);
delete_hop_by_hop([Hdr|Hdrs]) -> [Hdr | delete_hop_by_hop(Hdrs)].

add_connection_close_header(Hdrs) ->
    case lists:keymember(<<"connection">>, 1, Hdrs) of
        true -> Hdrs;
        false -> [{<<"connection">>, <<"close">>} | Hdrs]
    end.

add_connection_keepalive_header(Hdrs) ->
    case lists:keymember(<<"connection">>, 1, Hdrs) of
        true -> Hdrs;
        false -> [{<<"connection">>, <<"keep-alive">>} | Hdrs]
    end.

add_connection_upgrade_header(Hdrs) ->
    case lists:keymember(<<"connection">>, 1, Hdrs) of
        true -> Hdrs;
        false -> [{<<"connection">>, <<"Upgrade">>} | Hdrs]
    end.

add_via(Headers) ->
    Via = vegur_utils:get_via_value(),
    vegur_utils:add_or_append_header(<<"via">>, Via, Headers).
