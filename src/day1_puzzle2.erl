-module(day1_puzzle2).

-export([main/1]).

main(FileName) ->
    Snacks = puzzle1:read_file(FileName),
    ElfTotals = lists:map(fun puzzle1:return_elf_totals/1, Snacks),
    [Elf1, Elf2, Elf3 | _] = order_list(ElfTotals),
    Elf1 + Elf2 + Elf3.

order_list(List) ->
    lists:sort(
        fun(CaloriesA, CaloriesB) ->
            CaloriesA >= CaloriesB
        end,  List).