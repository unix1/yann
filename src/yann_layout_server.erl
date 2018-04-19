%%%-------------------------------------------------------------------
%% @doc `yann_layout_server' module
%%
%% Single process gen_server implementation of network layout server.
%% It acts as a proxy to handing out available spots in the network to newly
%% spawned neurons; and provides network address to pid translation.
%%
%% To do this it
%%
%% - stores network layout (layers and neurons per layer)
%% - stores neuron address (layer and index) mapping to its pid in neuron map
%% - monitors neuron pids and removes them from the neuron map when they crash
%% - adds new neuron pids to the neuron map when assigning them to the network
%% @end
%%%-------------------------------------------------------------------

-module(yann_layout_server).

-behaviour(gen_server).

% API
-export([assign_spot/0]).
-export([get_layout/0]).
-export([get_neuron_map/0]).
-export([get_process_map/0]).
-export([set_layout/1]).

% Supervision
-export([start_link/0, init/1]).

% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type state() :: #{
    status => status(),
    layout => yann_layout:layout(),
    neuron_map => neuron_map(),
    process_map => process_map()
}.
-type status() :: init | running.
-type neuron_map() :: [layer_neuron_map()].
-type layer_neuron_map() :: [pid()].

-type spot() :: {pos_integer(), pos_integer()}.
%% Address - layer and neuron index - of a neuron pid in a network

-type process_map() :: #{pid() => spot()}.
%% Map used to store pid to network spot translation; useful for quick lookups
%% when DOWN message is received during monitoring of neurons.

-ifdef(TEST).
-compile(export_all).
-endif.

%%====================================================================
%% Supervision
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
    State = #{
        status => init,
        layout => yann_layout:new([]),
        neuron_map => [],
        process_map => #{}
    },
    {ok, State}.

%%====================================================================
%% API
%%====================================================================

%% @doc Assign a spot to the calling process
%%
%% @end
-spec assign_spot() -> spot()|not_found.
assign_spot() ->
    gen_server:call(?MODULE, {assign_spot}).

%% @doc Get network layout
%%
%% @end
-spec get_layout() -> yann_layout:layout().
get_layout() ->
    gen_server:call(?MODULE, {get_layout}).

%% @doc Get current neuron map
%%
%% @end
-spec get_neuron_map() -> neuron_map().
get_neuron_map() ->
    gen_server:call(?MODULE, {get_neuron_map}).

%% @doc Get current process map
%%
%% @end
-spec get_process_map() -> process_map().
get_process_map() ->
    gen_server:call(?MODULE, {get_process_map}).

%% @doc Set initial layout of the network
%%
%% @end
-spec set_layout(Layout :: yann_layout:layout()) -> ok.
set_layout(Layout) ->
    ok = gen_server:call(?MODULE, {set_layout, Layout}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

-type from() :: {pid(), term()}.
-spec handle_call(term(), from(), state()) -> {reply, term(), state()}.
handle_call({get_layout}, _From, #{layout := Layout} = State) ->
    {reply, Layout, State};
handle_call({get_neuron_map}, _From, #{neuron_map := NeuronMap} = State) ->
    {reply, NeuronMap, State};
handle_call({get_process_map}, _From, #{process_map := ProcessMap} = State) ->
    {reply, ProcessMap, State};
handle_call({set_layout, Layout}, _From, #{status := init} = State) ->
    NeuronMap = create_neuron_map_from_layout(Layout),
    StateNew = State#{status := running, layout := Layout, neuron_map := NeuronMap},
    %% TODO trigger supervisor to start neuron workers
    {reply, ok, StateNew};
handle_call({assign_spot}, _From, #{status := init} = State) ->
    {reply, not_found, State};
handle_call(
    {assign_spot},
    {Pid, _},
    #{status := running, neuron_map := NeuronMap, process_map := ProcessMap} = State
) ->
    {Spot, NewNeuronMap} = assign_next_available_spot(NeuronMap, Pid),
    NewProcessMap = add_pid_to_process_map(Pid, Spot, ProcessMap),
    NewState = State#{neuron_map := NewNeuronMap, process_map := NewProcessMap},
    %% NOTE `Spot' may be equal to not_found
    {reply, Spot, NewState}.

-spec handle_cast(_, State) -> {noreply, State} when State::state().
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_, State) -> {noreply, State} when State::state().
handle_info(
    {'DOWN', _MonitorRef, process, Pid, _Reason},
    #{status := running, neuron_map := NeuronMap, process_map := ProcessMap} = State
) ->
    {LayerIndex, NeuronIndex} = maps:get(Pid, ProcessMap),
    NewNeuronMap = assign_spot_to_pid(LayerIndex, NeuronIndex, NeuronMap, none),
    NewProcessMap = maps:remove(Pid, ProcessMap),
    NewState = State#{neuron_map := NewNeuronMap, process_map := NewProcessMap},
    {noreply, NewState};
handle_info(_Msg, State) -> {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(_, State, _) -> {ok, State} when State::state().
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Create a new neuron map from a given layout
%% @private
%%
%% @end
-spec create_neuron_map_from_layout(yann_layout:layout()) -> neuron_map().
create_neuron_map_from_layout(Layout) ->
    NumberOfNeurons = yann_layout:get_number_of_neurons(Layout),
    Fun = fun(NeuronsInLayer, Acc) ->
        [lists:duplicate(NeuronsInLayer, none) | Acc]
    end,
    lists:foldr(Fun, [], NumberOfNeurons).

%% @doc Place the pid in the neuron map
%% @private
%%
%% Finds the next available spot in the neuron map, places the pid in that spot
%% and returns it along with the new neuron map.
%% @end
-spec assign_next_available_spot(neuron_map(), pid()) ->
    {spot() | not_found, neuron_map()}.
assign_next_available_spot(NeuronMap, Pid) when is_pid(Pid) ->
    case find_next_available_spot(NeuronMap) of
        not_found ->
            {not_found, NeuronMap};
        {LayerIndex, NeuronIndex} ->
            NewNeuronMap = assign_spot_to_pid(LayerIndex, NeuronIndex, NeuronMap, Pid),
            _Ref = erlang:monitor(process, Pid),
            {{LayerIndex, NeuronIndex}, NewNeuronMap}
    end.

%% @doc Find next available spot in the neuron map
%% @private
%%
%% @end
-spec find_next_available_spot(neuron_map()) -> spot()|not_found.
find_next_available_spot(NeuronMap) ->
    % List of first available position in each layer
    LayersUnassignedIndexes = [yann_util:list_pos(none, Layer, 1) || Layer <- NeuronMap],
    yann_util:list_non_pos(not_found, LayersUnassignedIndexes, 1).

%% @doc Assign a given Pid to the given layer and index
%% @private
%%
%% Returns a new neuron map with assignment in place.
%% @end
-spec assign_spot_to_pid(
    LayerIndex :: non_neg_integer(),
    NeuronIndex :: non_neg_integer(),
    NeuronMap :: neuron_map(),
    Pid :: pid()|none
) -> neuron_map().
assign_spot_to_pid(LayerIndex, NeuronIndex, NeuronMap, Pid) ->
    Layer = lists:nth(LayerIndex, NeuronMap),
    NewLayer = yann_util:list_setnth(NeuronIndex, Layer, Pid),
    NewNeuronMap = yann_util:list_setnth(LayerIndex, NeuronMap, NewLayer),
    NewNeuronMap.

%% @doc Add process to spot mapping to process map
%% @private
%%
%% @end
-spec add_pid_to_process_map(pid(), spot()|not_found, process_map()) -> process_map().
add_pid_to_process_map(_Pid, not_found, ProcessMap) ->
    ProcessMap;
add_pid_to_process_map(Pid, Spot, ProcessMap) ->
    ProcessMap#{Pid => Spot}.
