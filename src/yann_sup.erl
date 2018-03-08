%%%-------------------------------------------------------------------
%% @doc `yann_sup' module
%%
%% This is a top level `one_for_one' yann supervisor started by the yann
%% application. It, in turn, starts the following supervisors under it:
%% - {@link yann_neuron_sup}
%% - {@link yann_layout_sup}
%% @end
%%%-------------------------------------------------------------------

-module(yann_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/0, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args},
        permanent, 5000, Type, [Module]}).

%%====================================================================
%% Supervision
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{supervisor:strategy(), 0, 1}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, { {one_for_one, 0, 1}, children()} }.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Get children specs
%% @private
%%
%% A convenience function to return all children specs.
%% @end
-spec children() -> [supervisor:child_spec()].
children() ->
    NeuronSup = ?CHILD(yann_neuron_sup, yann_neuron_sup, [], supervisor),
    LayoutSup = ?CHILD(yann_layout_sup, yann_layout_sup, [], supervisor),
    [NeuronSup, LayoutSup].
