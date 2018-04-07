-module(yann_neuron_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%%====================================================================
%% Supervision
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{supervisor:strategy(), 1, 3600}, [supervisor:child_spec()]}}.
init([]) ->
    MaxRestart = 1,
    MaxTime = 3600,
    ChildSpec = {yann_neuron,
                 {yann_neuron, start_link, [self()]},
                 permanent,
                 5000, % shutdown time
                 worker,
                 [yann_neuron]},
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.
