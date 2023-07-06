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


-module(pgsqlp_replication).


-feature(maybe_expr, enable).


-export([expression/0]).
-import(pgsqlp, [kv/2]).
-import(pgsqlp, [t/1]).
-import(pgsqlp, [to_atom/1]).
-import(scran_branch, [alt/1]).
-import(scran_character_complete, [alpha1/0]).
-import(scran_character_complete, [hex_digit1/0]).
-import(scran_character_complete, [multispace0/0]).
-import(scran_character_complete, [multispace1/0]).
-import(scran_character_complete, [re/1]).
-import(scran_character_complete, [tag/1]).
-import(scran_character_complete, [tag_no_case/1]).
-import(scran_combinator, [ignore_result/1]).
-import(scran_combinator, [map_result/2]).
-import(scran_combinator, [opt/1]).
-import(scran_multi, [separated_list0/2]).
-import(scran_multi, [separated_list1/2]).
-import(scran_sequence, [delimited/3]).
-import(scran_sequence, [preceded/2]).
-import(scran_sequence, [separated_pair/3]).
-import(scran_sequence, [sequence/1]).


expression() ->
    alt([identify_system(),
         create_replication_slot(),
         drop_replication_slot(),
         start_replication()]).


identify_system() ->
    sequence([kv(action, to_atom(tag_no_case("IDENTIFY_SYSTEM")))]).


create_replication_slot() ->
    sequence(
      [kv(action, to_atom(tag_no_case("CREATE_REPLICATION_SLOT"))),
       slot_name(),
       opt(t(preceded(multispace1(), tag_no_case("TEMPORARY")))),
       alt([sequence(
              [kv(mode, preceded(multispace1(), to_atom(tag_no_case("LOGICAL")))),
               kv(output_plugin, preceded(multispace1(), alpha1()))]),
            sequence(
              [kv(mode, preceded(multispace1(), tag_no_case("PHYSICAL"))),
               opt(preceded(multispace1(), tag_no_case("TEMPORARY")))])]),
       alt([preceded(
              multispace1(),
              kv(
                snapshot,
                to_atom(
                  alt([tag_no_case("EXPORT_SNAPSHOT"),
                       tag_no_case("NOEXPORT_SNAPSHOT"),
                       tag_no_case("USE_SNAPSHOT")])))),

            preceded(
              multispace1(),
              delimited(
                tag("("),
                separated_list1(
                  sequence([multispace0(), tag(","), multispace0()]),
                  alt([map_result(
                         separated_pair(
                           to_atom(tag_no_case("SNAPSHOT")),
                           multispace1(),
                           delimited(
                             tag("'"),
                             to_atom(
                               alt([tag_no_case("export"),
                                    tag_no_case("use"),
                                    tag_no_case("nothing")])),
                             tag("'"))),
                         fun erlang:list_to_tuple/1)])),
                tag(")")))])]).


drop_replication_slot() ->
    sequence(
      [kv(action, to_atom(tag_no_case("DROP_REPLICATION_SLOT"))),
       slot_name(),
       opt(t(preceded(multispace1(), tag_no_case("WAIT"))))]).


start_replication() ->
    sequence(
      [kv(action, to_atom(tag_no_case("START_REPLICATION"))),
       ignore_result(preceded(multispace1(), tag_no_case("SLOT"))),
       slot_name(),
       kv(mode, preceded(multispace1(), to_atom(tag_no_case("LOGICAL")))),
       preceded(multispace1(), lsn()),
       preceded(multispace1(), options())]).


options() ->
    kv(?FUNCTION_NAME,
       delimited(
         tag("("),
         map_result(
           separated_list0(sequence([tag(","), multispace0()]),
                           option()),
           fun maps:from_list/1),
         tag(")"))).


option() ->
    map_result(
      separated_pair(to_atom(option_name()), multispace1(), option_value()),
      fun erlang:list_to_tuple/1).


option_name() ->
    re("[a-zA-Z_]+").


option_value() ->
    delimited(tag("'"), re("[^']+"), tag("'")).


lsn() ->
    kv(?FUNCTION_NAME,
       map_result(
         sequence([hex_digit1(), tag("/"), hex_digit1()]),
         fun erlang:list_to_binary/1)).


slot_name() ->
    kv(slot,
       preceded(
         multispace1(),
         alt(
           [quoted(re("[a-zA-Z0-9_]+")),
            re("[a-zA-Z0-9_]+")]))).


quoted(Parser) ->
    Quote = tag("\""),
    delimited(Quote, Parser, Quote).
