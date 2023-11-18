-module(day1_puzzle1).
-export([main/1, read_file/1, return_elf_totals/1]).


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

main(FileName) ->
    Snacks = read_file(FileName),
    ElfTotals = lists:map(fun return_elf_totals/1, Snacks),
    CaloriesTotal = lists:max(ElfTotals),
    io:format("The elf carries ~p calories\n", [CaloriesTotal]).



