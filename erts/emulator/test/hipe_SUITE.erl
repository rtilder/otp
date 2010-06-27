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
-export([call_beam_module_with_on_load/1]).

all(suite) ->
    [call_beam_module_with_on_load].

init_per_suite(Config) when is_list(Config) ->
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    {skip, "Native code support is not enabled"};
	_ ->
	    Config
    end.

end_per_suite(Config) when is_list(Config) ->
    ok.

call_beam_module_with_on_load(suite) ->
    [];
call_beam_module_with_on_load(doc) ->
    ["Test that we can call a non-native module with a on_load from a native module"];
call_beam_module_with_on_load(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line CallerModule = filename:join(DataDir, "native_caller.erl"),
    ?line CalleeModule = filename:join(DataDir, "beam_callee.erl"),
    ?line PrivDir = ?config(priv_dir, Config),
    test_call_with_on_load(PrivDir, CallerModule, CalleeModule).

test_call_with_on_load(PrivDir, CallerModule, CalleeModule) ->
    % Compile both modules.
    ?line {ok, CallerModuleName} = compile:file(CallerModule, [{outdir, PrivDir}]),
    ?line {ok, CalleeModuleName} = compile:file(CalleeModule, [{outdir, PrivDir}]),
    % Add PrivDir to the path so we can load modules interactively.
    AbsPrivDir = filename:absname(PrivDir),
    ?line true = code:add_patha(AbsPrivDir),
    % Call the function in caller module.
    ?line ok = CallerModuleName:call(CalleeModuleName),
    % Purge both modules now.
    ?line true = code:soft_purge(CallerModuleName),
    ?line true = code:soft_purge(CalleeModuleName),
    % Remove PrivDir from the path.
    ?line true = code:del_path(AbsPrivDir),
    ok.
