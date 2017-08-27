-module(yann_server).

-behaviour(gen_server).

-export([start_link/1, init/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

% temp
-export([initialize_weights/1, weighted_sum/2, sigmoid/1]).

-type state() :: #{weights => weights(), data => data()}.
-type weights() :: list(float()).
-type data() :: any().

%%%%% User functions %%%%%

start_link(Options) ->
    gen_server:start_link(?MODULE, {Options}, []).

-spec input(Pid :: pid(), I :: integer(), X :: float()) -> ok.
input(Pid, I, X) when is_pid(Pid), is_integer(I), is_float(X) ->
    ok = gen_server:call(Pid, {input, I, X}).

-spec error() -> ok.
error() ->
    % TODO (2) specify input for error
    % TODO (5) call gen_server to compute error and notify previous layer neurons
    ok = gen_server:call(Pid, {error}).

%%%%% Behavior functions %%%%%

-spec init(tuple()) -> {ok, state()}.
init({_Options}) ->
    % TODO (3) add options:
    %       - layer (input, hidden, output)
    %       - connected neuron Pids from previous layer
    %       - connected neuron Pids from next layer
    % TODO other options:
    %       - type of activation function
    %       - preset weights
    Weights = initialize_weights(11),
    State = #{weights => Weights},
    {ok, State}.

handle_call({input, I, X}, _From, S = #{weights := Weights, data := Data}) when is_integer(I), I > 0, I < length(Weights) ->
    % TODO (1) if I index is set, store it in queue, pop from queue when resetting
    Data_New = array:set(I, X, Data),
    S_New = case length(array:sparse_to_list(Data_New)) of
        length(Weights) ->
            Z = weighted_sum(Weights, array:to_list(Data_New)),
            A = sigmoid(Z),
            % TODO (4) send A to all connected neurons from next layer
            % resets data
            % TODO (1) pop from queue here for each I
            S#{data := array:new()};
        _ ->
            S#{data := Data_New}
    {reply, ok, S_New}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%%%% Library functions %%%%%

initialize_weights(N) ->
    initialize_weights(N, []).

initialize_weights(0, Acc) ->
    Acc;

initialize_weights(N, Acc) when N > 0 ->
    initialize_weights(N - 1, [rand:uniform() | Acc]).

weighted_sum(Theta, X) ->
    weighted_sum(Theta, X, 0).

weighted_sum([], [], Acc) ->
    Acc;

weighted_sum([Theta | Theta_rest], [X | X_rest], Acc) ->
    weighted_sum(Theta_rest, X_rest, Acc + Theta * X).

sigmoid(Z) ->
    1 / (1 + math:exp(-Z)).
