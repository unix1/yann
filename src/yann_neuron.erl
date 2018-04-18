%%%-------------------------------------------------------------------
%% @doc `yann_neuron' module
%%
%% Implementation of a single neuron as a process.
%% @end
%%%-------------------------------------------------------------------

-module(yann_neuron).

-behaviour(gen_server).

% API
-export([input/2, error/1]).

% Supervision
-export([start_link/1, init/1]).

% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type state() :: #{
    data => data(),
    data_queue => data_queue(),
    spot => yann_layout_server:spot(),
    weights => weights()
}.
-type weights() :: [float()].
-type data() :: array:array(). % array of floats
-type data_queue() :: array:array(). % array of queues
-type input() :: {pos_integer(), float()}.

-ifdef(TEST).
-compile(export_all).
-endif.

%%====================================================================
%% Supervision
%%====================================================================

-spec start_link(_) -> {ok, pid()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, {Options}, []).

-spec init({_}) -> {ok, state()}.
init({_Options}) ->
    % TODO (3) add options:
    %       - layer (input, hidden, output)
    %       - connected neuron Pids from previous layer
    %       - use number of inputs
    %       - connected neuron Pids from next layer
    % TODO (6) other options:
    %       - type of activation function
    %       - preset weights
    case yann_layout_server:assign_spot() of
        not_found ->
            {stop, no_spot};
        Spot ->
            NumberOfInputs = 10,
            Data = initialize_data(NumberOfInputs + 1),
            DataQueue = initialize_data_queue(NumberOfInputs),
            Weights = initialize_weights(NumberOfInputs + 1),
            State = #{data => Data, data_queue => DataQueue, spot => Spot, weights => Weights},
            {ok, State}
    end.

%%====================================================================
%% API
%%====================================================================

-spec input(pid(), input()) -> ok.
input(Pid, {I, X}) when is_pid(Pid), is_integer(I), I > 0, is_float(X) ->
    ok = gen_server:call(Pid, {input, {I, X}}).

-spec error(Pid :: pid()) -> ok.
error(Pid) ->
    % TODO (5) specify input for error
    % TODO (5) call gen_server to compute error and notify previous layer neurons
    ok = gen_server:call(Pid, {error}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

-type from() :: {pid(), term()}.
-spec handle_call({input, input()}, from(), state()) -> {reply, ok, state()}.
handle_call({input, {I, X}}, _From, State = #{weights := Weights, data := Data, data_queue := DataQueue})
        when is_integer(I), I > 0, I < length(Weights) ->
    {DataNew, DataQueueNew} = input_to_data_or_queue({I, X}, Data, DataQueue),
    Full_Data_Length = length(Weights),
    StateNew = case length(array:sparse_to_list(DataNew)) of
        Full_Data_Length ->
            Z = weighted_sum(Weights, array:to_list(DataNew)),
            _A = sigmoid(Z),
            % TODO (4) send A to all connected neurons from next layer
            % TODO (4) how to check/send again if ready after refill from queue?
            % TODO (4) probably create a recursive function that sends until not ready
            {DataNew, DataQueueNew} = initialize_data_from_data_queue(DataQueue),
            State#{data := DataNew, data_queue := DataQueueNew};
        _ ->
            State#{data := DataNew, data_queue := DataQueueNew}
    end,
    {reply, ok, StateNew}.

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

-spec initialize_data(pos_integer()) -> data().
initialize_data(N) when is_integer(N), N > 0 ->
    array:set(0, 1, array:new(N)).

-spec initialize_data_from_data_queue(data_queue()) -> {data(), data_queue()}.
initialize_data_from_data_queue(DataQueue) ->
    DataInitial = initialize_data(array:size(DataQueue) + 1),
    array:foldl(
        fun(Index, Queue, {DataAcc, DataQueueAcc}) ->
            case queue:is_empty(Queue) of
                true ->
                    {DataAcc, DataQueueAcc};
                _ ->
                    {{value, Item}, QueueNew} = queue:out(Queue),
                    DataQueueNew = array:set(Index, QueueNew, DataQueueAcc),
                    DataNew = array:set(Index + 1, Item, DataAcc),
                    {DataNew, DataQueueNew}
            end
        end,
        {DataInitial, DataQueue},
        DataQueue
    ).

-spec initialize_data_queue(pos_integer()) -> data_queue().
initialize_data_queue(N) when is_integer(N), N > 0 ->
    array:new(N, {default, queue:new()}).

-spec initialize_weights(pos_integer()) -> weights().
initialize_weights(N) when is_integer(N), N > 0 ->
    initialize_weights(N, []).

-spec initialize_weights(non_neg_integer(), [float()]) -> weights().
initialize_weights(0, Acc) when is_list(Acc) ->
    Acc;
initialize_weights(N, Acc) when is_integer(N), N > 0, is_list(Acc) ->
    initialize_weights(N - 1, [rand:uniform() | Acc]).

-spec input_to_data_or_queue(input(), data(), data_queue()) ->
    {data(), data_queue()}.
input_to_data_or_queue({I, X}, Data, DataQueue)
        when is_integer(I), I > 0, is_float(X) ->
    case array:get(I, Data) of
        undefined ->
            % If spot is open, set in Data
            {array:set(I, X, Data), DataQueue};
        _ ->
            % Otherwise, set in data queue
            {Data, append_input_to_data_queue({I, X}, DataQueue)}
    end.

-spec append_input_to_data_queue(input(), data_queue()) ->
    data_queue().
append_input_to_data_queue({I, X}, DataQueue)
        when is_integer(I), I > 0, is_float(X) ->
    Queue = array:get(I, DataQueue),
    array:set(I, queue:in(X, Queue), DataQueue).

-spec weighted_sum([float()], [float()]) -> float().
weighted_sum(Theta, X) when length(Theta) =:= length(X) ->
    weighted_sum(Theta, X, 0.0).

-spec weighted_sum([float()], [float()], float()) -> float().
weighted_sum([], [], Acc) when is_float(Acc) ->
    Acc;
weighted_sum(Theta = [Theta_current | Theta_rest], X = [X_current | X_rest], Acc)
        when is_float(Theta_current), is_float(X_current),
            length(Theta) =:= length(X), is_float(Acc) ->
    weighted_sum(Theta_rest, X_rest, Acc + Theta_current * X_current).

-spec sigmoid(float()) -> float().
sigmoid(Z) when is_float(Z) ->
    1 / (1 + math:exp(-Z)).
