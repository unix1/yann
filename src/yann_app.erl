-module(yann_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%%%%% Behavior functions %%%%%

start(_Type, _Args) ->
    yann_sup:start_link().

stop(_State) ->
    ok.
