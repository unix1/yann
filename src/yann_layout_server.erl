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
-export([set_layout/1]).

% Supervision
-export([start_link/0, init/1]).

% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type state() :: #{layout => yann_layout:layout(), neuron_map => neuron_map()}.
-type neuron_map() :: [layer_neuron_map()].
-type layer_neuron_map() :: [pid()].

-ifdef(TEST).
-compile(export_all).
-endif.

%%====================================================================
%% Supervision
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%%% Behavior functions %%%%%

-spec init([]) -> {ok, state()}.
init([]) ->
    State = #{layout => yann_layout:new([]), neuron_map => []},
    {ok, State}.

%%====================================================================
%% API
%%====================================================================
-spec set_layout(Layout :: yann_layout:layout()) -> ok.
set_layout(_Layout) ->
    ok.

%%====================================================================
%% Behavior callbacks
%%====================================================================

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
