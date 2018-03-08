%%%-------------------------------------------------------------------
%% @doc `yann_layout_sup' module
%%
%% This supervisor is started by {@link yann_sup} top level supervisor. It
%% supervises {@link yann_layout_server}.
%% @end
%%%-------------------------------------------------------------------

-module(yann_layout_sup).

-behaviour(supervisor).

% Supervision
-export([start_link/0, init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args},
        permanent, 5000, Type, [Module]}).

%%====================================================================
%% Supervision
%%====================================================================

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{supervisor:strategy(), 0, 1}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, { {one_for_one, 0, 1}, children()} }.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Get children specs
%% @private
%%
%% A convenience function to return all children specs.
%% @end
-spec children() -> [supervisor:child_spec()].
children() ->
    LayoutServer = ?CHILD(yann_layout_server, yann_layout_server, [], worker),
    [LayoutServer].
