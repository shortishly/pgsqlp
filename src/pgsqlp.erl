%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(pgsqlp).


-feature(maybe_expr, enable).


-export([kv/1]).
-export([kv/2]).
-export([name/0]).
-export([parse/1]).
-export([priv_consult/1]).
-export([priv_dir/0]).
-export([reserved/0]).
-export([start/0]).
-export([t/0]).
-export([t/1]).
-export([to_atom/1]).
-import(scran_branch, [alt/1]).
-import(scran_character_complete, [re_no_case/1]).
-import(scran_combinator, [is_not/1]).
-import(scran_combinator, [map_parser/2]).
-import(scran_combinator, [map_result/2]).


start() ->
    application:ensure_all_started(?MODULE).


priv_dir() ->
    code:priv_dir(?MODULE).


priv_consult(Filename) ->
    phrase_file:consult(filename:join(priv_dir(), Filename)).


parse(SQL) ->
    maybe
        {_, Result} ?= (alt([pgsqlp_select:expression(),
                             pgsqlp_copy:expression(),
                             pgsqlp_tx:expression(),
                             pgsqlp_replication:expression()]))(SQL),

        maps:from_list(Result)
    end.


kv(Key, Parser) ->
    fun
        (Input) ->
            (map_result(Parser, kv(Key)))(Input)
    end.

kv(Key) ->
    fun
        (Value) ->
            {Key, Value}
    end.


t(Parser) ->
    fun
        (Input) ->
            (map_result(
               to_atom(Parser),
               t()))(Input)
    end.


t() ->
    fun
        (Value) ->
            {Value, true}
    end.


to_atom(Parser) ->
    fun
        (Input) ->
            (map_result(
               Parser,
               fun
                   (Result) ->
                       binary_to_atom(
                         iolist_to_binary(
                           lists:join(
                             "_",
                             string:split(
                               string:lowercase(Result), " "))))
               end))(Input)
    end.


name() ->
    fun
        (Input) ->
            (map_parser(
               re_no_case("[a-z_]+"),
               is_not(
                 pgsqlp:reserved())))(Input)
    end.


reserved() ->
    fun
        (Input) ->
            (pgsqlp_keyword:by_category(reserved_keyword))(Input)
    end.
