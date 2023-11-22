-module(day1).
-export([puzzle1/1, puzzle2/1]).

read_file(FileName) ->
    {ok, RawInput} = file:read_file(FileName),
    RawData = binary:bin_to_list(RawInput),
    lists:map(
        fun(Snacks) ->
            string:split(Snacks, "\n", all)
        end, string:split(RawData, "\n\n", all)).

return_elf_totals(ElfSnacks) ->
    lists:foldl(
        fun(Elem, Sum) ->
            {IntElem, []} = string:to_integer(Elem),
            IntElem + Sum
        end, 0, ElfSnacks).

%%==============================================================================
%% Day 1 Puzzle 1
%%==============================================================================
-spec puzzle1(string()) -> integer().
puzzle1(FileName) ->
    Snacks = read_file(FileName),
    ElfTotals = lists:map(fun return_elf_totals/1, Snacks),
    CaloriesTotal = lists:max(ElfTotals),
    CaloriesTotal.

order_list(List) ->
    lists:sort(
        fun(CaloriesA, CaloriesB) ->
            CaloriesA >= CaloriesB
        end,  List).

%%==============================================================================
%% Day 1 Puzzle 2
%%==============================================================================
-spec puzzle2(string()) -> integer().
puzzle2(FileName) ->
    Snacks = read_file(FileName),
    ElfTotals = lists:map(fun return_elf_totals/1, Snacks),
    [Elf1, Elf2, Elf3 | _] = order_list(ElfTotals),
    Elf1 + Elf2 + Elf3.