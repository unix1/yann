-module(yann_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    yann_server_start/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        yann_server_start
    ].

init_per_suite(Config) ->
    ok = application:start(yann),
    Config.

end_per_suite(_) ->
    application:stop(yann),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

yann_server_start(_) ->
    {ok, _Pid} = supervisor:start_child(yann_sup, []).

