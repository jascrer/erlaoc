-module(day5_puzzle2).

-export([main/1]).

main(FileName) ->
    {Tuples, Moves} = puzzle1:read_stacks(FileName),
    MovedTuples = move_crates(Tuples, Moves),
    lists:sort(
        fun({Index1, _}, {Index2, _}) ->
            Index1 =< Index2
        end, MovedTuples).

move_crates(Tuples, []) ->
    Tuples;
move_crates(Tuples, [{Quantity, Stack1, Stack2} | Moves]) ->
    ResultTuple = move_crate(Tuples, Quantity, Stack1, Stack2),
    move_crates(ResultTuple, Moves).

move_crate(Tuples, 0, _, _) ->
    Tuples;
move_crate(Tuples, Quantity, Stack1, Stack2) ->
    {Tuple1, Tuple2, OtherTuples} = puzzle1:extract_tuples(Tuples, {Stack1, Stack2}),
    [Crate, TupleFrom] = move_crate_from(Tuple1, Quantity),
    TupleTo = move_crate_to(Tuple2, Crate),
    OtherTuples ++ [TupleFrom] ++ [TupleTo].

move_crate_from(Tuple, Quantity) ->
    move_crate_from(Tuple, Quantity, []).
move_crate_from(Tuple, 0, Acc)->
    [Acc, Tuple];
move_crate_from({Index, [Crate | Rest]}, Quantity, Acc) ->
    move_crate_from({Index, Rest}, Quantity - 1, Acc ++ [Crate]).
move_crate_to({Index, Crates}, Crate) ->
    {Index, Crate ++ Crates}.