-module(yann_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

%%%%% Behavior functions %%%%%

-spec start(_, _) -> {ok, pid()}.
start(_Type, _Args) ->
    yann_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.
