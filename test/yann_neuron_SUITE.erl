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

-spec all() -> [atom()].
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

-spec init_per_suite(yann_test_utils:config()) -> yann_test_utils:config().
init_per_suite(Config) ->
    ok = application:start(yann),
    Config.

-spec end_per_suite(yann_test_utils:config()) -> ok.
end_per_suite(_Config) ->
    application:stop(yann),
    ok.

-spec init_per_testcase(atom(), yann_test_utils:config()) -> yann_test_utils:config().
init_per_testcase(_, Config) ->
    Config.

-spec end_per_testcase(atom(), yann_test_utils:config()) -> ok.
end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

-spec get_layout() -> yann_layout:layout().
get_layout() ->
    yann_layout:new([
        #{type => 'input', number_of_neurons => 1},
        #{type => 'hidden', number_of_neurons => 2},
        #{type => 'output', number_of_neurons => 1}
    ]).

%%====================================================================
%% Unit Tests
%%====================================================================

-spec yann_neuron_append_input_to_data_queue_new(yann_test_utils:config()) -> ok.
yann_neuron_append_input_to_data_queue_new(_Config) ->
    DataQueue = array:set(1, queue:from_list([2, 3]), array:new(3, {default, queue:new()})),
    DataQueueActual = yann_neuron:append_input_to_data_queue({2, 5.5}, DataQueue),
    DataQueueExpected = array:set(2, queue:from_list([5.5]), array:set(1, queue:from_list([2, 3]), array:new(3, {default, queue:new()}))),
    DataQueueExpected = DataQueueActual,
    ok.

-spec yann_neuron_append_input_to_data_queue_existing(yann_test_utils:config()) -> ok.
yann_neuron_append_input_to_data_queue_existing(_Config) ->
    DataQueue = array:set(1, queue:from_list([2, 3]), array:new(3, {default, queue:new()})),
    DataQueueActual = yann_neuron:append_input_to_data_queue({1, 5.5}, DataQueue),
    DataQueueExpected = array:set(1, queue:from_list([2, 3, 5.5]), array:new(3, {default, queue:new()})),
    arrays_of_queues_equal(DataQueueActual, DataQueueExpected),
    ok.

-spec yann_neuron_initialize_data(yann_test_utils:config()) -> ok.
yann_neuron_initialize_data(_Config) ->
    Actual = yann_neuron:initialize_data(10),
    Expected = array:set(0, 1, array:new(10)),
    Expected = Actual,
    ok.

-spec yann_neuron_initialize_data_from_data_queue_empty(yann_test_utils:config()) -> ok.
yann_neuron_initialize_data_from_data_queue_empty(_Config) ->
    DataQueue = yann_neuron:initialize_data_queue(2),
    % Confirm nothing has changed
    DataExpected = yann_neuron:initialize_data(3),
    {DataExpected, DataQueue} = yann_neuron:initialize_data_from_data_queue(DataQueue),
    ok.

-spec yann_neuron_initialize_data_from_data_queue_existing(yann_test_utils:config()) -> ok.
yann_neuron_initialize_data_from_data_queue_existing(_Config) ->
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
    arrays_of_queues_equal(DataQueueExpected, DataQueueActual),
    ok.

-spec yann_neuron_initialize_data_queue(yann_test_utils:config()) -> ok.
yann_neuron_initialize_data_queue(_Config) ->
    Actual = yann_neuron:initialize_data_queue(10),
    Expected = array:new(10, {default, queue:new()}),
    Expected = Actual,
    ok.

-spec yann_neuron_initialize_weights(yann_test_utils:config()) -> ok.
yann_neuron_initialize_weights(_Config) ->
    Actual = yann_neuron:initialize_weights(1000),
    1000 = length(Actual),
    lists:foldl(
        fun(Value, ok) when Value >= 0, Value =< 1 ->
            ok
        end,
        ok,
        Actual
    ),
    ok.

-spec yann_neuron_input_to_data_or_queue_empty(yann_test_utils:config()) -> ok.
yann_neuron_input_to_data_or_queue_empty(_Config) ->
    Data = array:new(3),
    DataQueue = array:new(3, {default, queue:new()}),
    {DataActual, DataQueueActual} = yann_neuron:input_to_data_or_queue({1, 123.4}, Data, DataQueue),
    DataExpected = array:set(1, 123.4, array:new(3)),
    DataQueueExpected = array:new(3, {default, queue:new()}),
    DataExpected = DataActual,
    arrays_of_queues_equal(DataQueueExpected, DataQueueActual),
    ok.

-spec yann_neuron_input_to_data_or_queue_taken(yann_test_utils:config()) -> ok.
yann_neuron_input_to_data_or_queue_taken(_Config) ->
    Data = array:set(1, 111.1, array:new(3)),
    DataQueue = array:new(3, {default, queue:new()}),
    {DataActual, DataQueueActual} = yann_neuron:input_to_data_or_queue({1, 123.4}, Data, DataQueue),
    DataExpected = array:set(1, 111.1, array:new(3)),
    DataQueueExpected = array:set(1, queue:from_list([123.4]), array:new(3, {default, queue:new()})),
    DataExpected = DataActual,
    arrays_of_queues_equal(DataQueueExpected, DataQueueActual),
    ok.

-spec yann_neuron_sigmoid(yann_test_utils:config()) -> ok.
yann_neuron_sigmoid(_Config) ->
    1.0 = yann_neuron:sigmoid(99.9),
    0.5 = yann_neuron:sigmoid(0.0),
    ok.

-spec yann_neuron_weighted_sum(yann_test_utils:config()) -> ok.
yann_neuron_weighted_sum(_Config) ->
    2.0 = yann_neuron:weighted_sum([1.2, 2.0, 4.0, -1.0], [5.0, 3.0, -2.0, 2.0]),
    +0.0 = yann_neuron:weighted_sum([], []),
    ok.

%%====================================================================
%% Functional Tests
%%====================================================================

-spec yann_neuron_start(yann_test_utils:config()) -> ok.
yann_neuron_start(_Config) ->
    % Neuron should self-destruct when there are no spots in the network
    {error, no_spot} = supervisor:start_child(yann_neuron_sup, []),
    Layout = get_layout(),
    ok = yann_layout_server:set_layout(Layout),
    {ok, _Pid} = supervisor:start_child(yann_neuron_sup, []),
    ok.

%%====================================================================
%% Private
%%====================================================================

-spec arrays_of_queues_equal(array:array(queue:queue()), array:array(queue:queue())) -> ok.
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
