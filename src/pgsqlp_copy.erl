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


-module(pgsqlp_copy).


-feature(maybe_expr, enable).


-export([expression/0]).
-import(pgsqlp, [name/0]).
-import(pgsqlp, [t/1]).
-import(pgsqlp, [to_atom/1]).
-import(scran_branch, [alt/1]).
-import(scran_character_complete, [multispace1/0]).
-import(scran_character_complete, [tag/1]).
-import(scran_character_complete, [tag_no_case/1]).
-import(scran_combinator, [opt/1]).
-import(scran_multi, [separated_list1/2]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [delimited/3]).
-import(scran_sequence, [preceded/2]).
-import(scran_sequence, [sequence/1]).
-include_lib("kernel/include/logger.hrl").


expression() ->
    fun
        (Input) ->
            (sequence(
               [kv(action, to_atom(tag_no_case("COPY"))),
                alt(
                  [sequence(
                     [preceded(
                       multispace1(),
                       table_name()),
                      opt(preceded(
                            multispace1(),
                            delimited(tag("("),
                                      pgsqlp_scalar:column_references(),
                                      tag(")"))))]),
                   pgsqlp_scalar:expression()]),
                to()]))(Input)
             end.


to() ->
    fun
        (Input) ->
            (sequence(
               [preceded(
                  sequence([multispace1(),
                            tag_no_case("TO"),
                            multispace1()]),
                  alt([kv(filename, delimited(tag("'"), name(), tag("'"))),

                       kv(program,
                          preceded(
                            tag_no_case("PROGRAM"),
                            preceded(
                              multispace1(),
                              delimited(tag("'"), name(), tag("'"))))),

                       t(tag_no_case("STDOUT"))]))]))(Input)
    end.


table_name() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                separated_list1(
                  tag("."),
                  name())))(Input)
    end.
