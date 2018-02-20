%%%-------------------------------------------------------------------
%% @doc `yann_map_server' module
%%
%% Single process gen_server implementation of network map server.
%% It acts as a proxy to handing out available spots in the network to newly
%% spawned neurons; and provides network address to pid translation.
%%
%% To do this it
%%
%% - stores network map layout (layers and neurons per layer)
%% - stores neuron address (layer and index) mapping to its pid
%% - monitors neuron pids and removes them from map when they crash
%% @end
%%%-------------------------------------------------------------------
-module(yann_map_server).

-behaviour(gen_server).

-export([start_link/0, init/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type state() :: #{network_map => network_map(), neuron_map => neuron_map()}.
-type network_map() :: #{layers => [layer()]}.
-type layer() :: #{number_of_neurons => integer()}.
-type neuron_map() :: [layer_neuron_map()].
-type layer_neuron_map() :: [pid()].

-ifdef(TEST).
-compile(export_all).
-endif.

%%%%% User functions %%%%%

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%%% Behavior functions %%%%%

-spec init([]) -> {ok, state()}.
init([]) ->
    State = #{network_map => #{layers => []}, neuron_map => []},
    {ok, State}.

-type from() :: {pid(), term()}.
-spec handle_call(term(), from(), state()) -> {reply, ok, state()}.
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(_, State) -> {noreply, State} when State::state().
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_, State) -> {noreply, State} when State::state().
handle_info(_Msg, State) -> {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(_, State, _) -> {ok, State} when State::state().
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%%%% Library functions %%%%%
