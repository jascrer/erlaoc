-module(day4_puzzle2).

-export([main/1]).

main(FileName) ->
    Pairs = puzzle1:read_pairs(FileName),
    lists:foldl(fun(Pair, Sum) ->
            check_pair(Pair) + Sum
        end, 0, Pairs).

check_pair([{_LElf1, GElf1},{LElf2, _GElf2}]) when (GElf1 < LElf2) -> 0;
check_pair([{LElf1, _GElf1},{_LElf2, GElf2}]) when (GElf2 < LElf1) -> 0;
check_pair(_) -> 1.
