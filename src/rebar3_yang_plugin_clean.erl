-module(rebar3_yang_plugin_clean).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}]).

-include("rebar3_yang_plugin.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([{name,       ?PROVIDER},
                                 {module,     ?MODULE},
                                 {namespace,  yang},
                                 {bare,       true},
                                 {deps,       ?DEPS},
                                 {example,    "rebar3 yang clean"},
                                 {short_desc, "compile yang files."},
                                 {desc,       "compile yang files."},
                                 {opts,       []}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Opts = proplists:unfold(rebar_state:get(State, yang_opts, [])),
    YangBuildDir = proplists:get_value(yang_build_dir, Opts, ?YANGBUILDDIR),
    {rebar_file_utils:delete_each(filelib:wildcard(filename:join([YangBuildDir, "*.hrl"]))), State}.


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions 
%% ===================================================================
