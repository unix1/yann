-module(yann_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Unit Tests
-export([
    yann_start_neurons/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        % Functional
        yann_start_neurons
    ].

init_per_suite(Config) ->
    ok = application:start(yann),
    Config.

end_per_suite(_) ->
    application:stop(yann),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

get_layout() ->
    InputLayer = #{type => 'input', number_of_neurons => 3},
    HiddenLayer = #{type => 'hidden', number_of_neurons => 10},
    OutputLayer = #{type => 'output', number_of_neurons => 5},
    Layers = [
        InputLayer,
        HiddenLayer,
        OutputLayer
    ],
    yann_layout:new(Layers).

%%====================================================================
%% Unit Tests
%%====================================================================

yann_start_neurons(_) ->
    % Set layout and start neurons
    Layout = get_layout(),
    ok = yann:set_layout(Layout),
    Layout = yann_layout_server:get_layout(),
    ChildStartResults = yann:start_neurons(),
    ChildPids = [Pid || {ok, Pid} <- ChildStartResults],
    18 = length(ChildPids),
    % Make sure all neuron states contain spots
    FunGetPidState = fun(Pid) -> sys:get_state(Pid) end,
    ChildStates = lists:map(FunGetPidState, ChildPids),
    FunGetSpot = fun(#{spot := Spot}) -> Spot end,
    ChildSpots = lists:map(FunGetSpot, ChildStates),
    FunIsSpot = fun({LayerIndex, NeuronIndex}) ->
        is_integer(LayerIndex) and is_integer(NeuronIndex)
    end,
    ExpectedSpotValidation = lists:duplicate(18, true),
    ActualSpotValidation = lists:map(FunIsSpot, ChildSpots),
    ExpectedSpotValidation = ActualSpotValidation.
