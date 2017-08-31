-module(yann_server).

-behaviour(gen_server).

-export([start_link/1, init/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([input/3, error/1]).

% temp
-export([initialize_weights/1, weighted_sum/2, sigmoid/1, initialize_data_queue/1, append_input_to_data_queue/3, input_to_data_or_queue/3]).

-type state() :: #{weights => weights(), data => data(), data_queue => data()}.
-type weights() :: list(float()).
-type data() :: any(). % array

%%%%% User functions %%%%%

-spec start_link(_) -> {ok, pid()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, {Options}, []).

-spec input(Pid :: pid(), I :: integer(), X :: float()) -> ok.
input(Pid, I, X) when is_pid(Pid), is_integer(I), is_float(X) ->
    ok = gen_server:call(Pid, {input, I, X}).

-spec error(Pid :: pid()) -> ok.
error(Pid) ->
    % TODO (2) specify input for error
    % TODO (5) call gen_server to compute error and notify previous layer neurons
    ok = gen_server:call(Pid, {error}).

%%%%% Behavior functions %%%%%

-spec init(tuple()) -> {ok, state()}.
init({_Options}) ->
    % TODO (3) add options:
    %       - layer (input, hidden, output)
    %       - connected neuron Pids from previous layer
    %       - use number of inputs
    %       - connected neuron Pids from next layer
    % TODO other options:
    %       - type of activation function
    %       - preset weights
    NumberOfInputs = 10,
    DataQueue = initialize_data_queue(NumberOfInputs),
    Weights = initialize_weights(NumberOfInputs + 1),
    State = #{weights => Weights, data_queue => DataQueue},
    {ok, State}.

-type from() :: {pid(), term()}.
-spec handle_call({input, integer(), float()}, from(), state()) -> {reply, ok, state()}.
handle_call({input, I, X}, _From, State = #{weights := Weights, data := Data, data_queue := DataQueue})
        when is_integer(I), I > 0, I < length(Weights) - 1 ->
    [DataNew, DataQueueNew] = input_to_data_or_queue({I, X}, Data, DataQueue),
    Full_Data_Length = get_weights_length(State) - 1,
    State_New = case length(array:sparse_to_list(DataNew)) of
        Full_Data_Length ->
            Z = weighted_sum(Weights, array:to_list(DataNew)),
            _A = sigmoid(Z),
            % TODO (4) send A to all connected neurons from next layer
            % resets data
            % TODO (1) pop from queue here for each I
            State#{data := array:new()};
        _ ->
            State#{data := DataNew, data_queue := DataQueueNew}
    end,
    {reply, ok, State_New}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(_, state()) -> {noreply, state()}.
handle_info(_Msg, State) -> {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(_, state(), _) -> {ok, state()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%%%% State functions %%%%%

-spec get_weights_length(state()) -> non_neg_integer().
get_weights_length(_State = #{weights := Weights}) ->
    length(Weights).

%%%%% Library functions %%%%%

-spec initialize_weights(non_neg_integer()) -> [float()].
initialize_weights(N) when N > 0 ->
    initialize_weights(N, []).

-spec initialize_weights(non_neg_integer(), array:array()) -> [float()].
initialize_weights(0, Acc) ->
    Acc;
initialize_weights(N, Acc) when N > 0 ->
    initialize_weights(N - 1, [rand:uniform() | Acc]).

-spec initialize_data_queue(non_neg_integer()) -> array:array().
initialize_data_queue(N) ->
    array:new(N, {default, queue:new()}).

-spec input_to_data_or_queue({non_neg_integer(), float()}, array:array(), array:array()) ->
    [array:array()].
input_to_data_or_queue({I, X}, Data, DataQueue) ->
    % If spot is open, set in Data
    case array:get(I, Data) of
        undefined ->
            [array:set(I, X, Data), DataQueue];
        _ ->
            [Data, append_input_to_data_queue(I, X, DataQueue)]
    end.

-spec append_input_to_data_queue(non_neg_integer(), float(), array:array()) ->
    array:array().
append_input_to_data_queue(I, X, DataQueue) ->
    Queue = array:get(I, DataQueue),
    array:set(I, queue:in(X, Queue), DataQueue).

-spec weighted_sum([float()], [float()]) -> float().
weighted_sum(Theta, X) ->
    weighted_sum(Theta, X, 0).

-spec weighted_sum([float()], [float()], float()) -> float().
weighted_sum([], [], Acc) ->
    Acc;
weighted_sum([Theta | Theta_rest], [X | X_rest], Acc) ->
    weighted_sum(Theta_rest, X_rest, Acc + Theta * X).

-spec sigmoid(float()) -> float().
sigmoid(Z) ->
    1 / (1 + math:exp(-Z)).
