-module(yann_util_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Unit Tests
-export([
    yann_util_list_pos/1,
    yann_util_list_non_pos/1
]).

%%====================================================================
%% ct functions
%%====================================================================

-spec all() -> [atom()].
all() ->
    [
        % Unit
        yann_util_list_pos,
        yann_util_list_non_pos
    ].

-spec init_per_suite(yann_test_utils:config()) -> yann_test_utils:config().
init_per_suite(Config) ->
    Config.

-spec end_per_suite(yann_test_utils:config()) -> ok.
end_per_suite(_Config) ->
    ok.

-spec init_per_testcase(atom(), yann_test_utils:config()) -> yann_test_utils:config().
init_per_testcase(_, Config) ->
    Config.

-spec end_per_testcase(atom(), yann_test_utils:config()) -> ok.
end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Unit Tests
%%====================================================================

-spec yann_util_list_pos(yann_test_utils:config()) -> ok.
yann_util_list_pos(_Config) ->
    3 = yann_util:list_pos(c, [a, b, c, d, e], 1),
    2 = yann_util:list_pos(c, [a, b, c, d, e], 0),
    not_found = yann_util:list_pos(g, [a, b, c, d, e], 0),
    ok.

-spec yann_util_list_non_pos(yann_test_utils:config()) -> ok.
yann_util_list_non_pos(_Config) ->
    {4, d} = yann_util:list_non_pos(c, [c, c, c, d, e], 1),
    {3, d} = yann_util:list_non_pos(c, [c, c, c, d, e], 0),
    {0, a} = yann_util:list_non_pos(c, [a, b, c, d, e], 0),
    not_found = yann_util:list_non_pos(g, [g, g, g, g, g], 0),
    ok.
