-module(yann_test_utils).

-export([make_layout/0]).

-type config() :: [{atom(), term()}].

-export_type([config/0]).

-spec make_layout() -> yann_layout:layout().
make_layout() ->
    yann_layout:new([
        #{type => 'input', number_of_neurons => 3},
        #{type => 'hidden', number_of_neurons => 10},
        #{type => 'output', number_of_neurons => 5}
    ]).
