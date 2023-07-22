#-*- mode: makefile-gmake -*-
# Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

PROJECT = pgsqlp
PROJECT_DESCRIPTION = PostgreSQL Parser
PROJECT_VERSION = 0.1.0

COVER = 1

COVER_REPORT_DIR = _site/cover
CT_LOGS_DIR = _site/ct
EDOC_OPTS = {preprocess, true}, {dir, "_site/edoc"}

DEPS = \
	envy \
	phrase \
	scran

SHELL_DEPS = \
	sync

SHELL_OPTS = \
	+pc unicode \
	-config dev.config \
	-enable-feature maybe_expr \
	-s $(PROJECT) \
	-s sync

EUNIT_ERL_OPTS = \
	-enable-feature maybe_expr

PLT_APPS = \
	any \
	asn1 \
	compiler \
	crypto \
	inets \
	mnesia \
	phrase \
	public_key \
	runtime_tools \
	scran \
	ssl \
	stdlib \
	syntax_tools \
	tools \
	xmerl

dep_envy = git https://github.com/shortishly/envy.git
dep_phrase = git https://github.com/shortishly/phrase.git
dep_scran = git https://github.com/shortishly/scran.git


dep_envy_commit = 0.7.2
dep_phrase_commit = 0.1.0
dep_scran_commit = main


include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
