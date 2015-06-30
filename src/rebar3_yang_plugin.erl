-module(rebar3_yang_plugin).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_yang_plugin_compile:init(State),
    {ok, State2} = rebar3_yang_plugin_clean:init(State1),
    {ok, State2}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State}.


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions 
%% ===================================================================
