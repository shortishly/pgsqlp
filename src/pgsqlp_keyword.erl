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


-module(pgsqlp_keyword).


-export([by_category/1]).
-export([cache/0]).
-import(scran_branch, [alt/1]).
-import(scran_character_complete, [re_no_case/1]).
-on_load(on_load/0).


-type keyword() :: unicode:chardata().

-type category() :: col_name_keyword
                  | reserved_keyword
                  | type_func_name_keyword
                  | unreserved_keyword.


-type is_bare_label() :: bare_label
                       | as_label.


-spec cache() -> [{keyword(), atom(), category(), is_bare_label()}].

cache() ->
    persistent_term:get(?MODULE).


-spec by_category(category()) -> scran:parser().

by_category(Desired) ->
    fun
        (Input) ->
            (alt(
               [re_no_case(Keyword ++ "\\b") ||
                   {Keyword, _, Category, _} <- cache(),
                   Desired == Category]))(Input)
    end.


on_load() ->
    persistent_term:put(
      ?MODULE,
      pgsqlp:priv_consult("keyword.terms")).
