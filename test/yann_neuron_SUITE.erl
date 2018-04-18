-module(yann_neuron_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Unit Tests
-export([
    yann_neuron_append_input_to_data_queue_new/1,
    yann_neuron_append_input_to_data_queue_existing/1,
    yann_neuron_initialize_data/1,
    yann_neuron_initialize_data_from_data_queue_empty/1,
    yann_neuron_initialize_data_from_data_queue_existing/1,
    yann_neuron_initialize_data_queue/1,
    yann_neuron_initialize_weights/1,
    yann_neuron_input_to_data_or_queue_empty/1,
    yann_neuron_input_to_data_or_queue_taken/1,
    yann_neuron_sigmoid/1,
    yann_neuron_weighted_sum/1
]).

%% Functional Tests
-export([
    yann_neuron_start/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        % Unit
        yann_neuron_append_input_to_data_queue_new,
        yann_neuron_append_input_to_data_queue_existing,
        yann_neuron_initialize_data,
        yann_neuron_initialize_data_from_data_queue_empty,
        yann_neuron_initialize_data_from_data_queue_existing,
        yann_neuron_initialize_data_queue,
        yann_neuron_initialize_weights,
        yann_neuron_input_to_data_or_queue_empty,
        yann_neuron_input_to_data_or_queue_taken,
        yann_neuron_sigmoid,
        yann_neuron_weighted_sum,
        % Functional
        yann_neuron_start
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
    InputLayer = #{type => 'input', number_of_neurons => 1},
    HiddenLayer = #{type => 'hidden', number_of_neurons => 2},
    OutputLayer = #{type => 'output', number_of_neurons => 1},
    Layers = [
        InputLayer,
        HiddenLayer,
        OutputLayer
    ],
    yann_layout:new(Layers).

%%====================================================================
%% Unit Tests
%%====================================================================

yann_neuron_append_input_to_data_queue_new(_) ->
    DataQueue = array:set(1, queue:from_list([2, 3]), array:new(3, {default, queue:new()})),
    DataQueueActual = yann_neuron:append_input_to_data_queue({2, 5.5}, DataQueue),
    DataQueueExpected = array:set(2, queue:from_list([5.5]), array:set(1, queue:from_list([2, 3]), array:new(3, {default, queue:new()}))),
    DataQueueExpected = DataQueueActual.

yann_neuron_append_input_to_data_queue_existing(_) ->
    DataQueue = array:set(1, queue:from_list([2, 3]), array:new(3, {default, queue:new()})),
    DataQueueActual = yann_neuron:append_input_to_data_queue({1, 5.5}, DataQueue),
    DataQueueExpected = array:set(1, queue:from_list([2, 3, 5.5]), array:new(3, {default, queue:new()})),
    arrays_of_queues_equal(DataQueueActual, DataQueueExpected).

yann_neuron_initialize_data(_) ->
    Actual = yann_neuron:initialize_data(10),
    Expected = array:set(0, 1, array:new(10)),
    Expected = Actual.

yann_neuron_initialize_data_from_data_queue_empty(_) ->
    DataQueue = yann_neuron:initialize_data_queue(2),
    % Confirm nothing has changed
    DataExpected = yann_neuron:initialize_data(3),
    {DataExpected, DataQueue} = yann_neuron:initialize_data_from_data_queue(DataQueue).

yann_neuron_initialize_data_from_data_queue_existing(_) ->
    DataQueue0 = yann_neuron:initialize_data_queue(3),
    DataQueue1 = array:set(0, queue:in(3.0, queue:in(2.0, queue:in(1.0, queue:new()))), DataQueue0),
    DataQueue2 = array:set(1, queue:new(), DataQueue1),
    DataQueue3 = array:set(2, queue:in(5.0, queue:new()), DataQueue2),
    DataQueue = DataQueue3,
    DataExpected0 = yann_neuron:initialize_data(4),
    DataExpected1 = array:set(1, 1.0, DataExpected0),
    DataExpected2 = array:set(2, undefined, DataExpected1),
    DataExpected3 = array:set(3, 5.0, DataExpected2),
    DataExpected = DataExpected3,
    DataQueueExpected0 = yann_neuron:initialize_data_queue(3),
    DataQueueExpected1 = array:set(0, queue:in(3.0, queue:in(2.0, queue:new())), DataQueueExpected0),
    DataQueueExpected2 = array:set(1, queue:new(), DataQueueExpected1),
    DataQueueExpected3 = array:set(2, queue:new(), DataQueueExpected2),
    DataQueueExpected = DataQueueExpected3,
    {DataActual, DataQueueActual} = yann_neuron:initialize_data_from_data_queue(DataQueue),
    DataExpected = DataActual,
    arrays_of_queues_equal(DataQueueExpected, DataQueueActual).

yann_neuron_initialize_data_queue(_) ->
    Actual = yann_neuron:initialize_data_queue(10),
    Expected = array:new(10, {default, queue:new()}),
    Expected = Actual.

yann_neuron_initialize_weights(_) ->
    Actual = yann_neuron:initialize_weights(1000),
    1000 = length(Actual),
    lists:foldl(
        fun(Value, ok) when Value >= 0, Value =< 1 ->
            ok
        end,
        ok,
        Actual
    ).

yann_neuron_input_to_data_or_queue_empty(_) ->
    Data = array:new(3),
    DataQueue = array:new(3, {default, queue:new()}),
    {DataActual, DataQueueActual} = yann_neuron:input_to_data_or_queue({1, 123.4}, Data, DataQueue),
    DataExpected = array:set(1, 123.4, array:new(3)),
    DataQueueExpected = array:new(3, {default, queue:new()}),
    DataExpected = DataActual,
    arrays_of_queues_equal(DataQueueExpected, DataQueueActual).

yann_neuron_input_to_data_or_queue_taken(_) ->
    Data = array:set(1, 111.1, array:new(3)),
    DataQueue = array:new(3, {default, queue:new()}),
    {DataActual, DataQueueActual} = yann_neuron:input_to_data_or_queue({1, 123.4}, Data, DataQueue),
    DataExpected = array:set(1, 111.1, array:new(3)),
    DataQueueExpected = array:set(1, queue:from_list([123.4]), array:new(3, {default, queue:new()})),
    DataExpected = DataActual,
    arrays_of_queues_equal(DataQueueExpected, DataQueueActual).

yann_neuron_sigmoid(_) ->
    1.0 = yann_neuron:sigmoid(99.9),
    0.5 = yann_neuron:sigmoid(0.0).

yann_neuron_weighted_sum(_) ->
    2.0 = yann_neuron:weighted_sum([1.2, 2.0, 4.0, -1.0], [5.0, 3.0, -2.0, 2.0]),
    0.0 = yann_neuron:weighted_sum([], []).

%%====================================================================
%% Functional Tests
%%====================================================================

yann_neuron_start(_) ->
    % Neuron should self-destruct when there are no spots in the network
    {error, no_spot} = supervisor:start_child(yann_neuron_sup, []),
    Layout = get_layout(),
    ok = yann_layout_server:set_layout(Layout),
    {ok, _Pid} = supervisor:start_child(yann_neuron_sup, []).

%%====================================================================
%% Private
%%====================================================================

arrays_of_queues_equal(Array1, Array2) ->
    ArraySize1 = array:size(Array1),
    ArraySize2 = array:size(Array2),
    ArraySize1 = ArraySize2,
    ok = array:foldl(
        fun(Index, Value, Acc) ->
            ValueList = queue:to_list(Value),
            ValueList = queue:to_list(array:get(Index, Array2)),
            Acc
        end,
        ok,
        Array1
    ).
