%%%-------------------------------------------------------------------
%% @doc `yann_layout' module
%%
%% This module is used to operate on the {@link yann_layout:layout()} data
%% type. This data type is used when setting network map structure in
%% {@link yann_layout_server:set_layout/1}. It describes number of layers in
%% a network, type of each layer, and number of neurons in each layer.
%% @end
%%%-------------------------------------------------------------------

-module(yann_layout).

%% API
-export([new/1]).
-export([get_number_of_layers/1]).

%% Types
-export_type([layout/0]).

-opaque layout() :: [layer()].
%% Data type used to set network layout structure. Use functions in this module
%% to operate on this data type.

-type layer() :: #{type => layer_type(), number_of_neurons => integer()}.
-type layer_type() :: input | hidden | output.

%%====================================================================
%% API
%%====================================================================

%% @doc Create a new {@link yann_layout:layout()} data type
%%
%% `Layout' argument must be in the form of a list of layers.
%% @end
-spec new([layer()]) -> layout().
new(Layers) when is_list(Layers) ->
    % Fold instead of simple assignment to validate input
    F = fun(#{type := Type, number_of_neurons := N}, Acc) ->
            [#{type => Type, number_of_neurons => N}|Acc]
        end,
    lists:foldr(F, [], Layers).

%% @doc Get number of layers
%%
%% Get number of layers in {@link yann_layout:layout()} data type.
%% @end
-spec get_number_of_layers(Layout :: layout()) -> pos_integer().
get_number_of_layers(Layout) ->
    length(Layout).
