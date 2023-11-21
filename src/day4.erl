-module(day4).

-export([puzzle1/1, puzzle2/1]).

puzzle1(FileName) ->
    Pairs = read_pairs(FileName),
    lists:foldl(fun(Pair, Sum) ->
            p1_check_pair(Pair) + Sum
        end, 0, Pairs).

read_pairs(FileName) ->
    {ok, RawInput} = file:read_file(FileName),
    RawData = binary:bin_to_list(RawInput),
    PairsData = string:split(RawData, "\n", all),
    Pairs = lists:map(
        fun(PairData) ->
            [Elf1, Elf2] = string:split(PairData, ",", all),
            [LElf1, GElf1] = string:split(Elf1,"-", all),
            [LElf2, GElf2] = string:split(Elf2,"-", all),
            [format_pairs(LElf1, GElf1),
                format_pairs(LElf2, GElf2)]
        end, PairsData),
    Pairs.

format_pairs(Lowest,Greatest) ->
    {LInteger, _} = string:to_integer(Lowest),
    {GInteger, _} = string:to_integer(Greatest),
    {LInteger, GInteger}.

p1_check_pair([{LElf1, GElf1},{LElf2, GElf2}]) when (LElf1 =< LElf2) and (GElf1 >= GElf2) -> 1;
p1_check_pair([{LElf1, GElf1},{LElf2, GElf2}]) when (LElf2 =< LElf1) and (GElf2 >= GElf1) -> 1;
p1_check_pair(_) -> 0.

puzzle2(FileName) ->
    Pairs = read_pairs(FileName),
    lists:foldl(fun(Pair, Sum) ->
            p2_check_pair(Pair) + Sum
        end, 0, Pairs).

p2_check_pair([{_LElf1, GElf1},{LElf2, _GElf2}]) when (GElf1 < LElf2) -> 0;
p2_check_pair([{LElf1, _GElf1},{_LElf2, GElf2}]) when (GElf2 < LElf1) -> 0;
p2_check_pair(_) -> 1.