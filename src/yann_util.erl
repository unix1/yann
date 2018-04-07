%%%-------------------------------------------------------------------
%% @doc `yann_util' module
%%
%% Convenience functions
%% @end
%%%-------------------------------------------------------------------

-module(yann_util).

% API
-export([list_pos/3]).
-export([list_non_pos/3]).
-export([list_setnth/3]).

%%====================================================================
%% API
%%====================================================================

%% @doc Returns the position of the value in the list or `false' if not found
%%
%% The `Base' argument specifies base non-negative integer for counting. This
%% is commonly 0 or 1.
%% @end
-spec list_pos(V :: term(), List :: list(), Base :: non_neg_integer()) ->
    non_neg_integer() |
    not_found.
list_pos(_, [], _Acc) -> not_found;
list_pos(V, [V|_Rest], Acc) -> Acc;
list_pos(V, [_|Rest], Acc) -> list_pos(V, Rest, Acc + 1).

%% @doc Returns the position of the non-matching value in the list or `false'
%% if all values equal `V'.
%%
%% The `Base' argument specifies base non-negative integer for counting. This
%% is commonly 0 or 1.
%% @end
-spec list_non_pos(V :: term(), List :: list(), Base :: non_neg_integer()) ->
    {non_neg_integer(), term()} |
    not_found.
list_non_pos(_, [], _Acc) -> not_found;
list_non_pos(V, [V|Rest], Acc) -> list_non_pos(V, Rest, Acc + 1);
list_non_pos(_, [W|_Rest], Acc) -> {Acc, W}.

%% @doc Set nth element of a list to given value
%%
%% Credit: Robert Virding https://stackoverflow.com/a/4781219
%% @end
-spec list_setnth(I :: pos_integer(), List :: list(), New :: term()) -> list().
list_setnth(1, [_|Rest], New) -> [New|Rest];
list_setnth(I, [E|Rest], New) -> [E|list_setnth(I-1, Rest, New)].
