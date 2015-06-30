-module(rebar3_yang_plugin_compile).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).

-include_lib("providers/include/providers.hrl").
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
                                 {example,    "rebar3 yang compile"},
                                 {short_desc, "compile yang files."},
                                 {desc,       "compile yang files."},
                                 {opts,       []}]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running yang...", []),
    Opts = proplists:unfold(rebar_state:get(State, yang_opts, [])),
    YangDir = proplists:get_value(yang_dir, Opts, ?YANGDIR),
    YangBuildDir = proplists:get_value(yang_build_dir, Opts, ?YANGBUILDDIR),
    NewConfig = rebar_state:set(State, erl_opts, 
                                 [{i, YangBuildDir}] ++ rebar_utils:erl_opts(State)),
    rebar_base_compiler:run(NewConfig, filelib:wildcard(filename:join([YangDir, "*.yang"])),
                            YangDir, ".yang", YangBuildDir, ".hrl", fun compile_yang/3),
    {ok, State}.


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions 
%% ===================================================================
-spec compile_yang(In :: file:filename(), Out :: file:filename(), Config :: rebar_state:t()) ->
    ok | {error, Error :: term()}.
compile_yang(In, Out, _Config) ->
    case yang:deep_parse_file(In) of
        {ok, Yang} ->
            Ts = yang:typespec(Yang),
            ok = filelib:ensure_dir(Out), 
            file:write_file(Out, yang_typespec:hrl(Ts));
        {error, Error} = E->
            rebar_api:error("Error: ~p~n", [Error]), E
    end.
