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


-module(pgsqlp_select).


-feature(maybe_expr, enable).


-export([expression/0]).
-import(pgsqlp, [t/1]).
-import(pgsqlp, [to_atom/1]).
-import(scran_branch, [alt/1]).
-import(scran_character_complete, [multispace0/0]).
-import(scran_character_complete, [multispace1/0]).
-import(scran_character_complete, [re/1]).
-import(scran_character_complete, [re_no_case/1]).
-import(scran_character_complete, [tag/1]).
-import(scran_character_complete, [tag_no_case/1]).
-import(scran_combinator, [is_not/1]).
-import(scran_combinator, [map_parser/2]).
-import(scran_combinator, [opt/1]).
-import(scran_multi, [separated_list1/2]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [delimited/3]).
-import(scran_sequence, [preceded/2]).
-import(scran_sequence, [sequence/1]).
-include_lib("kernel/include/logger.hrl").


%% SELECT pg_catalog.set_config('search_path', '', false);
expression() ->
    fun
        (Input) ->
            (sequence(
               [kv(action, to_atom(tag_no_case("SELECT"))),
                opt(t(preceded(multispace1(),
                               alt([tag_no_case("DISTINCT"),
                                    tag_no_case("ALL")])))),
                opt(kv(output,
                       preceded(
                         multispace1(),
                         separated_list1(
                           sequence([multispace0(), tag(","), multispace0()]),
                           pgsqlp_scalar:expression())))),

                opt(preceded(multispace1(), from())),
                opt(preceded(multispace1(), where())),

                opt(preceded(multispace1(), order_by()))]))(Input)
    end.

preceded_by(First, Second) ->
    fun
        (Input) ->
            (preceded(First, preceded(multispace1(), Second)))(Input)
    end.

order_by() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                preceded_by(
                  tag_no_case("ORDER BY"),
                  pgsqlp_scalar:expression())))(Input)
    end.

where() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                preceded(
                  tag_no_case("WHERE"),
                  preceded(
                    multispace1(),
                    pgsqlp_scalar:expression()))))(Input)
    end.

from() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                preceded(
                  tag_no_case("FROM"),
                  preceded(
                    multispace1(),
                    separated_list1(
                      sequence([multispace0(),
                                tag(","),
                                multispace0()]),
                      from_item())))))(Input)
    end.


from_item() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                alt([sequence(
                       [table_name(),
                        opt(
                          sequence(
                            [preceded(
                               multispace1(),
                               join_type()),
                             preceded(
                               multispace1(),
                               from_item()),
                             preceded(
                               multispace1(),
                               on())]))]),

                     sequence(
                       [opt(t(tag_no_case("LATERAL"))),
                        preceded(
                          multispace0(),
                          sequence(
                            [pgsqlp_scalar:function_call(),
                             opt(table_alias())]))])])))(Input)
    end.

on() ->
    fun
        (Input) ->
            (preceded(
               tag_no_case("ON"),
               preceded(
                 multispace1(),
                 delimited(
                   tag("("),
                   pgsqlp_scalar:expression(),
                   tag(")")))))(Input)
    end.


join_type() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                alt([re_no_case("(INNER )?JOIN"),
                     re_no_case("LEFT (OUTER )?JOIN"),
                     re_no_case("RIGHT (OUTER )?JOIN"),
                     re_no_case("FULL (OUTER )?JOIN")])))(Input)
    end.


table_name() ->
    fun
        (Input) ->
            (sequence(
               [kv(?FUNCTION_NAME,
                   separated_list1(
                     tag("."),
                     name())),
                opt(table_alias())]))(Input)
    end.


table_alias() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                preceded(
                  sequence(
                    [multispace1(),
                     opt(sequence(
                           [tag_no_case("AS"),
                            multispace1()]))]),
                  name())))(Input)
    end.


name() ->
    fun
        (Input) ->
            (map_parser(
               re("[a-z_]+"),
               is_not(pgsqlp:reserved())))(Input)
    end.
