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


-module(pgsqlp_tx).


-export([expression/0]).
-import(pgsqlp, [to_atom/1]).
-import(scran_branch, [alt/1]).
-import(scran_character_complete, [multispace1/0]).
-import(scran_character_complete, [tag_no_case/1]).
-import(scran_combinator, [ignore_result/1]).
-import(scran_combinator, [opt/1]).
-import(scran_multi, [separated_list1/2]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [preceded/2]).
-import(scran_sequence, [sequence/1]).


expression() ->
    fun
        (Input) ->
            (alt([begin_expression(),
                  commit_expression()]))(Input)
    end.

begin_expression() ->
    sequence(
      [kv(action, to_atom(tag_no_case("BEGIN"))),
       ignore_result(
         opt(preceded(
               multispace1(),
               alt([tag_no_case("WORK"), tag_no_case("TRANSACTION")])))),
       opt(preceded(
             multispace1(),
             separated_list1(
               multispace1(),
               alt([kv(isolation_level,
                       preceded(
                         sequence([tag_no_case("ISOLATION LEVEL"),
                                   multispace1()]),
                         to_atom(
                           alt([tag_no_case("SERIALIZABLE"),
                                tag_no_case("REPEATABLE READ"),
                                tag_no_case("READ COMMITTED"),
                                tag_no_case("READ UNCOMMITTED")])))),

                    kv(read_level,
                       to_atom(
                         alt([tag_no_case("READ WRITE"),
                              tag_no_case("READ ONLY")]))),

                    kv(deferrable,
                       to_atom(
                         alt([tag_no_case("NOT DEFERRABLE"),
                              tag_no_case("DEFERRABLE")])))]))))]).

commit_expression() ->
    sequence([kv(action, to_atom(tag_no_case("COMMIT")))]).
