%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(hipe_SUITE).

-include("test_server.hrl").

%% Test server specific exports
-export([all/1, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([load_non_native/1, load_non_sticky/1,
	 load_sticky/1, load_non_loaded/1]).

all(suite) ->
    [load_non_native, load_non_sticky, load_sticky, load_non_loaded].

init_per_suite(Config) when is_list(Config) ->
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    {skip, "Native code support is not enabled"};
	_ ->
	    Config
    end.

end_per_suite(Config) when is_list(Config) ->
    ok.

load_non_native(suite) ->
    [];
load_non_native(doc) ->
    ["Test hipe:load/1 with a non-native module"];
load_non_native(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line PrivDirFullPath = filename:absname(PrivDir),
    ?line code:add_patha(PrivDirFullPath),
    ?line NativeModuleSource = filename:join(DataDir, "non_native_module.erl"),

    ?line ok = compile_threaded_and_save(PrivDirFullPath, NativeModuleSource),
    ?line {error, {error, beam_lib, {missing_chunk, _, _}}} = hipe:load(non_native_module),
    ?line false = code:is_module_native(non_native_module),
    ?line true = code:del_path(PrivDirFullPath),
    ok.

load_non_sticky(suite) ->
    [];
load_non_sticky(doc) ->
    ["Test hipe:load/1 with a regular module"];
load_non_sticky(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line PrivDirFullPath = filename:absname(PrivDir),
    ?line code:add_patha(PrivDirFullPath),
    ?line NativeModuleSource = filename:join(DataDir, "native_module.erl"),

    ?line ok = compile_native_no_load(PrivDirFullPath, NativeModuleSource),
    ?line ok = compile_threaded_and_load(PrivDirFullPath, NativeModuleSource),
    ?line {module, native_module} = hipe:load(native_module),
    ?line true = code:is_module_native(native_module),
    ?line true = code:del_path(PrivDirFullPath),
    ok.

load_sticky(suite) ->
    [];
load_sticky(doc) ->
    ["Test hipe:load/1 with a sticky module"];
load_sticky(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line PrivDirFullPath = filename:absname(PrivDir),
    ?line code:add_patha(PrivDirFullPath),
    ?line NativeModuleSource = filename:join(DataDir, "sticky_native_module.erl"),

    ?line ok = compile_native_no_load(PrivDirFullPath, NativeModuleSource),
    ?line ok = compile_threaded_and_load(PrivDirFullPath, NativeModuleSource),
    ?line ok = code:stick_dir(PrivDirFullPath),
    ?line true = code:is_sticky(sticky_native_module),
    ?line {module, sticky_native_module} = hipe:load(sticky_native_module),
    ?line true = code:is_module_native(sticky_native_module),
    ?line true = code:is_sticky(sticky_native_module),
    ?line true = code:del_path(PrivDirFullPath),
    ok.

load_non_loaded(suite) ->
    [];
load_non_loaded(doc) ->
    ["Test hipe:load/1 with a module that was not previously loaded"];
load_non_loaded(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    ?line PrivDirFullPath = filename:absname(PrivDir),
    ?line code:add_patha(PrivDirFullPath),
    ?line SourcePath = filename:join(DataDir, "native_module_not_loaded.erl"),

    ?line ok = compile_native_no_load(PrivDirFullPath, SourcePath),
    ?line {module, native_module_not_loaded} = hipe:load(native_module_not_loaded),
    ?line true = code:is_module_native(native_module_not_loaded),
    ?line true = code:del_path(PrivDirFullPath),
    ok.

compile_native_no_load(PrivDirFullPath, SourcePath) ->
    ?line {ok, Module, []} = compile:file(SourcePath, [native, {outdir, PrivDirFullPath}, return, {hipe, [no_load]}]),
    ?line false = lists:keyfind(Module, 1, code:all_loaded()),
    ok.

compile_threaded_and_save(PrivDirFullPath, SourcePath) ->
    ?line {ok, Module, []} = compile:file(SourcePath, [{outdir, PrivDirFullPath}, return]),
    ?line false = lists:keyfind(Module, 1, code:all_loaded()),
    ?line BeamPath = filename:join([PrivDirFullPath, atom_to_list(Module) ++ ".beam"]),
    ?line {module, Module} = code:load_file(Module),
    ?line {Module, BeamPath} = lists:keyfind(Module, 1, code:all_loaded()),
    ?line false = code:is_module_native(Module),
    ok.

compile_threaded_and_load(PrivDirFullPath, SourcePath) ->
    ?line {ok, Module, Binary, []} = compile:file(SourcePath, [return, binary]),
    ?line false = lists:keyfind(Module, 1, code:all_loaded()),
    ?line BeamPath = filename:join([PrivDirFullPath, atom_to_list(Module) ++ ".beam"]),
    ?line {module, Module} = code:load_binary(Module, BeamPath, Binary),
    ?line {Module, BeamPath} = lists:keyfind(Module, 1, code:all_loaded()),
    ?line false = code:is_module_native(Module),
    ok.
