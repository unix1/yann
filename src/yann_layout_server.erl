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
%% - stores neuron address (layer and index) mapping to its pid
%% - monitors neuron pids and removes them from mapping when they crash
%% - adds new neuron pids to the mapping when assigning them network addresses
%% @end
%%%-------------------------------------------------------------------

-module(yann_layout_server).

-behaviour(gen_server).

% API
-export([get_layout/0]).
-export([get_neuron_map/0]).
-export([set_layout/1]).

% Supervision
-export([start_link/0, init/1]).

% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type state() :: #{
    status => status(),
    layout => yann_layout:layout(),
    neuron_map => neuron_map()
}.
-type status() :: init | running.
-type neuron_map() :: [layer_neuron_map()].
-type layer_neuron_map() :: [pid()].
-type spot() :: {pos_integer(), pos_integer()}.

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
    State = #{status => init, layout => yann_layout:new([]), neuron_map => []},
    {ok, State}.

%%====================================================================
%% API
%%====================================================================

-spec set_layout(Layout :: yann_layout:layout()) -> ok.
set_layout(Layout) ->
    ok = gen_server:call(?MODULE, {set_layout, Layout}).

-spec get_layout() -> yann_layout:layout().
get_layout() ->
    gen_server:call(?MODULE, {get_layout}).

-spec get_neuron_map() -> neuron_map().
get_neuron_map() ->
    gen_server:call(?MODULE, {get_neuron_map}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

-type from() :: {pid(), term()}.
-spec handle_call(term(), from(), state()) -> {reply, term(), state()}.
handle_call({get_layout}, _From, #{layout := Layout} = State) ->
    {reply, Layout, State};
handle_call({get_neuron_map}, _From, #{neuron_map := NeuronMap} = State) ->
    {reply, NeuronMap, State};
handle_call({set_layout, Layout}, _From, #{status := init} = State) ->
    NeuronMap = get_new_neuron_map_from_layout(Layout),
    StateNew = State#{status := running, layout := Layout, neuron_map := NeuronMap},
    %% TODO trigger supervisor to start neuron workers
    {reply, ok, StateNew};
handle_call({assign_spot}, _From, #{layout := Layout, neuron_map := NeuronMap} = State) ->
    {Spot, NewNeuronMap} = find_next_available_spot(Layout, NeuronMap),
    NewState = State#{neuron_map := NewNeuronMap},
    {reply, Spot, NewState}.

-spec handle_cast(_, State) -> {noreply, State} when State::state().
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_, State) -> {noreply, State} when State::state().
handle_info(_Msg, State) -> {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(_, State, _) -> {ok, State} when State::state().
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_new_neuron_map_from_layout(yann_layout:layout()) -> neuron_map().
get_new_neuron_map_from_layout(Layout) ->
    NumberOfNeurons = yann_layout:get_number_of_neurons(Layout),
    Fun = fun(NeuronsInLayer, Acc) ->
        [lists:duplicate(NeuronsInLayer, none) | Acc]
    end,
    lists:foldr(Fun, [], NumberOfNeurons).

-spec find_next_available_spot(yann_layout:layout(), neuron_map()) ->
    {spot() | none, neuron_map()}.
find_next_available_spot(_Layout, NeuronMap) ->
    %% TODO Find the next spot and return coordinates and updated neuron map
    {none, NeuronMap}.
