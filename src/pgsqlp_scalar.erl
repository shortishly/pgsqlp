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


-module(pgsqlp_scalar).
-feature(maybe_expr, enable).


-export([column_references/0]).
-export([expression/0]).
-export([function_call/0]).
-import(scran_branch, [alt/1]).
-import(scran_character_complete, [digit1/0]).
-import(scran_character_complete, [multispace0/0]).
-import(scran_character_complete, [multispace1/0]).
-import(scran_character_complete, [re/1]).
-import(scran_character_complete, [tag/1]).
-import(scran_character_complete, [tag_no_case/1]).
-import(scran_combinator, [is_not/1]).
-import(scran_combinator, [map_parser/2]).
-import(scran_combinator, [map_result/2]).
-import(scran_combinator, [opt/1]).
-import(scran_multi, [many1/1]).
-import(scran_multi, [separated_list0/2]).
-import(scran_multi, [separated_list1/2]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [delimited/3]).
-import(scran_sequence, [pair/2]).
-import(scran_sequence, [preceded/2]).
-import(scran_sequence, [separated_pair/3]).
-import(scran_sequence, [sequence/1]).
-import(scran_sequence, [terminated/2]).
-include_lib("kernel/include/logger.hrl").


expression() ->
    fun
        (Input) ->
            (alt([in_expression(),
                  operator_invocation(),
                  function_call(),
                  column_reference(),
                  conditional(),
                  type_cast(),
                  group_subexpression(),
                  literal_value()]))(Input)
    end.

group_subexpression() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                delimited(
                  sequence([multispace0(),
                            tag("("),
                            multispace0()]),
                  expression(),
                  sequence([multispace0(),
                            tag(")")]))))(Input)

    end.

conditional() ->
    fun
        (Input) ->
            (alt([case_expression()]))(Input)
    end.

case_expression() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                terminated(
                  preceded(
                    sequence(
                      [multispace0(),
                       tag_no_case("CASE")]),

                    sequence(
                      [kv(clauses,
                          many1(
                            sequence(
                              [kv(condition,
                                  preceded(
                                    sequence(
                                      [multispace1(),
                                       tag_no_case("WHEN"),
                                       multispace1()]),
                                    expression())),

                               kv(then,
                                  preceded(
                                    sequence(
                                      [multispace1(),
                                       tag_no_case("THEN"),
                                       multispace1()]),
                                    expression()))]))),

                       opt(kv(otherwise,
                              preceded(
                                sequence(
                                  [multispace1(),
                                   tag_no_case("ELSE"),
                                   multispace1()]),
                                expression())))])),
                  sequence(
                    [multispace1(),
                     tag_no_case("END"),
                     multispace0()]))))(Input)
    end.

type_cast() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                sequence(
                  [alt([literal_value()]),
                   preceded(
                     tag_no_case("::"),
                     type_name())])))(Input)
    end.


function_call() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                sequence([function_name(), function_args()])))(Input)
    end.


function_name() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
               alt([map_result(
                      separated_pair(schema_name(), tag("."), name()),
                      fun erlang:list_to_tuple/1),

                    tag_no_case("ANY"),

                    name()])))(Input)
    end.


function_args() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                delimited(
                  tag("("),
                  separated_list0(
                    sequence([tag(","), multispace0()]),
                    expression()),
                  tag(")"))))(Input)
    end.


operator_invocation() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME, infix_operator()))(Input)
    end.


infix_operator() ->
    fun
        (Input) ->
            (infix_operator(
               preceded(
                 multispace0(),
                 operator()),

               preceded(
                 multispace0(),
                 alt([function_call(),
                      in_expression(),
                      type_cast(),
                      column_reference(),
                      group_subexpression(),
                      literal_value()]))))(Input)
    end.


infix_operator(OperatorParser, ExpressionParser) ->
    fun
        (Input) ->
            ?LOG_DEBUG(
               #{input => Input,
                 operator_parser => scran_debug:pp(OperatorParser),
                 expression_parser => scran_debug:pp(ExpressionParser)}),

            maybe
                {OperatorInput, LHS} ?= ExpressionParser(Input),
                {RHSInput, Operator} ?=  OperatorParser(OperatorInput),
                {NextIterationInput, RHS} ?= ExpressionParser(RHSInput),


                (kv(?FUNCTION_NAME,
                    ?FUNCTION_NAME(
                       OperatorParser,
                       ExpressionParser,
                       [RHS, {operator, Operator}, LHS])))(NextIterationInput)
            end
    end.

infix_operator(OperatorParser, ExpressionParser, A) ->
    fun
        (Input) ->
            ?LOG_DEBUG(
               #{input => Input,
                 operator_parser => scran_debug:pp(OperatorParser),
                 expression_parser => scran_debug:pp(ExpressionParser),
                 a => A}),

            maybe
                {ExpressionInput, Operator} ?=  OperatorParser(Input),
                {NextIterationInput, Expression} ?= ExpressionParser(ExpressionInput),

                ?LOG_DEBUG(#{operator => Operator, expression => Expression}),

                (?FUNCTION_NAME(
                    OperatorParser,
                    ExpressionParser,
                    [Expression, {operator, Operator} | A]))(NextIterationInput)

            else
                nomatch ->
                    {Input, lists:reverse(A)}
            end
    end.

operator() ->
    fun
        (Input) ->
            (alt(
               [re("[\\+\\-\\*\\/\\<\\>\\=\\~\\!\\@\\#\\%\\^\\&\\|\\`\\?]{1,63}"),
                tag_no_case("AND NOT"),
                tag_no_case("AND"),
                tag_no_case("OR"),
                tag_no_case("NOT")]))(Input)
    end.


column_references() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
               separated_list1(
                 sequence([tag(","), multispace0()]),
                 column_reference())))(Input)
    end.


column_reference() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
               map_result(
                 separated_list1(
                   tag("."),
                   name()),
                 fun erlang:list_to_tuple/1)))(Input)
    end.


in_expression() ->
    fun
        (Input) ->
            (kv(in,
                pair(
                  column_references(),
                  preceded(
                    sequence(
                      [multispace1(),
                       tag_no_case("IN"),
                       multispace1()]),
                    delimited(
                      sequence([tag("("), multispace0()]),
                      separated_list0(
                        sequence([tag(","), multispace0()]),
                        expression()),
                      sequence([multispace0(), tag(")")]))))))(Input)
    end.


literal_value() ->
    fun
        (Input) ->
            (alt([null_literal(),
                  string_literal(),
                  numeric_literal(),
                  boolean_literal()]))(Input)
    end.


null_literal() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME, tag_no_case("null")))(Input)
    end.

string_literal() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                delimited(
                  tag("'"),
                  re("[a-z_]*"),
                  tag("'"))))(Input)
    end.

numeric_literal() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                map_result(
                  digit1(),
                  fun erlang:binary_to_integer/1)))(Input)
    end.

boolean_literal() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                map_result(
                  alt([tag_no_case("true"), tag_no_case("false")]),
                  fun
                      (Term) ->
                          binary_to_existing_atom(string:lowercase(Term))
                  end)))(Input)
    end.

type_name() ->
    fun
        (Input) ->
            (kv(?FUNCTION_NAME,
                separated_list0(
                  tag("."),
                  re("[a-z_][a-z0-9_]*"))))(Input)
    end.


schema_name() ->
    fun
        (Input) ->
            (name())(Input)
    end.


name() ->
    fun
        (Input) ->
            (map_parser(
               re("[a-z_]+"),
               is_not(pgsqlp:reserved())))(Input)
    end.
