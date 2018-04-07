%%%-------------------------------------------------------------------
%% @doc `yann_app' module
%%
%% This starts the yann application.
%% @end
%%%-------------------------------------------------------------------

-module(yann_app).

-behaviour(application).

%% Supervision
-export([start/2, stop/1]).

%%====================================================================
%% Supervision
%%====================================================================

-spec start(_, _) -> {ok, pid()}.
start(_Type, _Args) ->
    yann_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
    ok.
