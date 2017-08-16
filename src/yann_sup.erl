-module(yann_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%%%% Behavior functions %%%%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
