-module(yann_layout_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    yann_layout_get_number_of_layers/1,
    yann_layout_get_number_of_neurons/1,
    yann_layout_new/1,
    yann_layout_server_assign_next_available_spot/1,
    yann_layout_server_assign_spot/1,
    yann_layout_server_assign_spot_to_pid/1,
    yann_layout_server_create_neuron_map_from_layout/1,
    yann_layout_server_find_next_available_spot/1,
    yann_layout_server_init/1,
    yann_layout_server_set_layout/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        yann_layout_get_number_of_layers,
        yann_layout_get_number_of_neurons,
        yann_layout_new,
        yann_layout_server_assign_next_available_spot,
        yann_layout_server_assign_spot,
        yann_layout_server_assign_spot_to_pid,
        yann_layout_server_create_neuron_map_from_layout,
        yann_layout_server_find_next_available_spot,
        yann_layout_server_init,
        yann_layout_server_set_layout
    ].

init_per_suite(Config) ->
    ok = application:start(yann),
    Config.

end_per_suite(_) ->
    application:stop(yann),
    ok.

init_per_testcase(_, Config) ->
    % Will get {error, running} the first time, but that's ok
    supervisor:restart_child(yann_layout_sup, yann_layout_server),
    Config.

end_per_testcase(_, _Config) ->
    % Needed to start layout server with a clean slate
    supervisor:terminate_child(yann_layout_sup, yann_layout_server),
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

get_empty_neuron_map() ->
    [
        [none, none, none],
        [none, none, none, none, none, none, none, none, none, none],
        [none, none, none, none, none]
    ].

get_mixed_neuron_map() ->
    Pid = get_pid(),
    [
        lists:duplicate(3, Pid),
        [Pid, Pid, none, Pid, Pid, none, Pid, Pid, Pid, Pid],
        lists:duplicate(5, Pid)
    ].

get_full_neuron_map() ->
    Pid = get_pid(),
    [
        lists:duplicate(3, Pid),
        lists:duplicate(10, Pid),
        lists:duplicate(5, Pid)
    ].

get_pid() ->
    list_to_pid("<0.12345.0>").

%% @doc Returns a function acting like a neuron requesting a spot in the network
%% @private
%%
%% Useful when running tests against layout server and validating responses.
%% @end
get_neuron_fun() ->
    fun TestNeuronLoop() ->
        receive
            {From, assign_spot} ->
                Spot = yann_layout_server:assign_spot(),
                From ! Spot,
                TestNeuronLoop();
            {From, stop} ->
                From ! ok
        end
    end.

%%====================================================================
%% Tests
%%====================================================================

yann_layout_get_number_of_layers(_) ->
    Layout = get_layout(),
    3 = yann_layout:get_number_of_layers(Layout).

yann_layout_get_number_of_neurons(_) ->
    Layout = get_layout(),
    [3, 10, 5] = yann_layout:get_number_of_neurons(Layout).

yann_layout_new(_) ->
    Layout = get_layout(),
    3 = yann_layout:get_number_of_layers(Layout),
    10 = yann_layout:get_number_of_neurons(Layout, 2).

yann_layout_server_assign_next_available_spot(_) ->
    Pid = get_pid(),
    NewPid = list_to_pid("<0.123.0>"),
    % Test assigning to empty neuron map
    NeuronMap1 = get_empty_neuron_map(),
    {Spot1, NewNeuronMap1} = yann_layout_server:assign_next_available_spot(NeuronMap1, NewPid),
    ExpectedSpot1 = {1, 1},
    ExpectedNeuronMap1 = [
        [NewPid, none, none],
        [none, none, none, none, none, none, none, none, none, none],
        [none, none, none, none, none]
    ],
    ExpectedNeuronMap1 = NewNeuronMap1,
    ExpectedSpot1 = Spot1,
    % Test assigning to partially filled neuron map
    NeuronMap2 = get_mixed_neuron_map(),
    {Spot2, NewNeuronMap2} = yann_layout_server:assign_next_available_spot(NeuronMap2, NewPid),
    ExpectedSpot2 = {2, 3},
    ExpectedNeuronMap2 = [
        lists:duplicate(3, Pid),
        [Pid, Pid, NewPid, Pid, Pid, none, Pid, Pid, Pid, Pid],
        lists:duplicate(5, Pid)
    ],
    ExpectedNeuronMap2 = NewNeuronMap2,
    ExpectedSpot2 = Spot2,
    % Test assigning to full neuron map
    NeuronMap3 = get_full_neuron_map(),
    {Spot3, NewNeuronMap3} = yann_layout_server:assign_next_available_spot(NeuronMap3, NewPid),
    ExpectedSpot3 = not_found,
    ExpectedNeuronMap3 = get_full_neuron_map(),
    ExpectedNeuronMap3 = NewNeuronMap3,
    ExpectedSpot3 = Spot3.

yann_layout_server_assign_spot(_) ->
    Layout = get_layout(),
    ok = yann_layout_server:set_layout(Layout),
    TestNeuronFun = get_neuron_fun(),
    % Spawn 18 neurons to fill the network + 1 neuron to test response when full
    TestNeuronPids = spawn_many(TestNeuronFun, 18, []),
    TestNeuronPidFull = spawn(TestNeuronFun),
    AssignSpotFun = fun(Pid) ->
        Pid ! {self(), assign_spot},
        receive
            Spot -> Spot
            after 1 -> ct:fail("No response received from test neuron process")
        end
    end,
    NeuronSpots = [[AssignSpotFun(Pid)] || Pid <- TestNeuronPids],
    % Check server state to make sure pid gets assigned to maps
    18 = maps:size(yann_layout_server:get_process_map()),
    NeuronMap1 = yann_layout_server:get_neuron_map(),
    ExpectedNeuronMap1L1 = lists:sublist(TestNeuronPids, 1, 3),
    ExpectedNeuronMap1L2 = lists:sublist(TestNeuronPids, 4, 10),
    ExpectedNeuronMap1L3 = lists:sublist(TestNeuronPids, 14, 5),
    ExpectedNeuronMap1L1 = lists:nth(1, NeuronMap1),
    ExpectedNeuronMap1L2 = lists:nth(2, NeuronMap1),
    ExpectedNeuronMap1L3 = lists:nth(3, NeuronMap1),
    % Now the network should be fully populated, make sure next process gets not_found
    not_found = AssignSpotFun(TestNeuronPidFull),
    % Send messages to terminate test neuron processes
    StopNeuronFun = fun(Pid) ->
        Pid ! {self(), stop},
        receive
            ok -> ok
            after 1 -> ct:fail("No response received on stop from test neuron process")
        end
    end,
    ok = lists:foreach(StopNeuronFun, [TestNeuronPidFull|TestNeuronPids]),
    % Verify server state is updated accordingly
    ExpectedNeuronMap2 = [
        [none, none, none],
        [none, none, none, none, none, none, none, none, none, none],
        [none, none, none, none, none]
    ],
    ExpectedProcessMap2 = #{},
    NeuronMap2 = yann_layout_server:get_neuron_map(),
    ProcessMap2 = yann_layout_server:get_process_map(),
    ExpectedNeuronMap2 = NeuronMap2,
    ExpectedProcessMap2 = ProcessMap2.

yann_layout_server_assign_spot_to_pid(_) ->
    NeuronMap = get_empty_neuron_map(),
    Pid = list_to_pid("<0.123.0>"),
    % Test assigning layer 1 position 3
    NewNeuronMap1 = yann_layout_server:assign_spot_to_pid(1, 3, NeuronMap, Pid),
    ExpectedNeuronMap1 = [
        [none, none, Pid],
        [none, none, none, none, none, none, none, none, none, none],
        [none, none, none, none, none]
    ],
    ExpectedNeuronMap1 = NewNeuronMap1,
    % Test assigning layer 3 position 1
    NewNeuronMap2 = yann_layout_server:assign_spot_to_pid(3, 1, NeuronMap, Pid),
    ExpectedNeuronMap2 = [
        [none, none, none],
        [none, none, none, none, none, none, none, none, none, none],
        [Pid, none, none, none, none]
    ],
    ExpectedNeuronMap2 = NewNeuronMap2.

yann_layout_server_create_neuron_map_from_layout(_) ->
    Layout = get_layout(),
    NeuronMap = get_empty_neuron_map(),
    NeuronMap = yann_layout_server:create_neuron_map_from_layout(Layout).

yann_layout_server_find_next_available_spot(_) ->
    NeuronMap1 = get_empty_neuron_map(),
    {1, 1} = yann_layout_server:find_next_available_spot(NeuronMap1),
    NeuronMap2 = get_full_neuron_map(),
    not_found = yann_layout_server:find_next_available_spot(NeuronMap2),
    NeuronMap3 = get_mixed_neuron_map(),
    {2, 3} = yann_layout_server:find_next_available_spot(NeuronMap3).

yann_layout_server_init(_) ->
    ExpectedLayout = yann_layout:new([]),
    ExpectedNeuronMap = [],
    ExpectedProcessMap = #{},
    ExpectedLayout = yann_layout_server:get_layout(),
    ExpectedNeuronMap = yann_layout_server:get_neuron_map(),
    ExpectedProcessMap = yann_layout_server:get_process_map().

yann_layout_server_set_layout(_) ->
    Layout = get_layout(),
    ok = yann_layout_server:set_layout(Layout),
    Layout = yann_layout_server:get_layout(),
    NeuronMap = yann_layout_server:get_neuron_map(),
    ExpectedNeuronMap = get_empty_neuron_map(),
    ExpectedNeuronMap = NeuronMap.

%%====================================================================
%% Private
%%====================================================================

spawn_many(_Fun, 0, Acc) ->
    Acc;
spawn_many(Fun, N, Acc) when N > 0 ->
    spawn_many(Fun, N - 1, [spawn(Fun)|Acc]).
