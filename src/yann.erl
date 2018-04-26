%%%-------------------------------------------------------------------
%% @doc `yann' module
%%
%% This module acts as an API consisting of convenient functions to interact
%% with the application.
%% @end
%%%-------------------------------------------------------------------

-module(yann).

%% API
-export([set_layout/1]).
-export([start_neurons/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Set initial layout of the network
%%
%% @end
-spec set_layout(Layout :: yann_layout:layout()) -> ok.
set_layout(Layout) ->
    ok = yann_layout_server:set_layout(Layout).

%% @doc Start neuron processes
%%
%% Starts `yann_neuron' processes equal to the number of neurons in the layout.
%% This is useful when called after initial {@link yann:set_layout()}.
%% @end
-spec start_neurons() -> [supervisor:startchild_ret()].
start_neurons() ->
    Layout = yann_layout_server:get_layout(),
    NumberOfNeurons = lists:sum(yann_layout:get_number_of_neurons(Layout)),
    yann_util:sup_start_children(yann_neuron_sup, [], NumberOfNeurons).
