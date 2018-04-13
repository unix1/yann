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

all() ->
    [
        % Unit
        yann_util_list_pos,
        yann_util_list_non_pos
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Helpers
%%====================================================================

%%====================================================================
%% Unit Tests
%%====================================================================

yann_util_list_pos(_) ->
    3 = yann_util:list_pos(c, [a, b, c, d, e], 1),
    2 = yann_util:list_pos(c, [a, b, c, d, e], 0),
    not_found = yann_util:list_pos(g, [a, b, c, d, e], 0).

yann_util_list_non_pos(_) ->
    {4, d} = yann_util:list_non_pos(c, [c, c, c, d, e], 1),
    {3, d} = yann_util:list_non_pos(c, [c, c, c, d, e], 0),
    {0, a} = yann_util:list_non_pos(c, [a, b, c, d, e], 0),
    not_found = yann_util:list_non_pos(g, [g, g, g, g, g], 0).
