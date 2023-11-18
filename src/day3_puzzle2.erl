-module(day3_puzzle2).

-export([main/1]).

main(FileName) ->
    Rucksacks = read_rucksacks(FileName),
    RepeatedItems = lists:map(
        fun({C1, C2, C3}) ->
            search_repeated(C1, C2, C3)
        end, Rucksacks),
    puzzle1:calculate_points(RepeatedItems).

read_rucksacks(FileName) ->
    {ok, RawInput} = file:read_file(FileName),
    RawData = binary:bin_to_list(RawInput),
    RucksacksData = string:split(RawData, "\n", all),
    Rucksacks = group_rucksacks(RucksacksData),
    Rucksacks.

group_rucksacks(Rucksacks)->
    group_rucksacks1(Rucksacks, []).

group_rucksacks1([], Groups) ->
    Groups;
group_rucksacks1([R1, R2, R3 | Rest], Groups) ->
    group_rucksacks1(Rest, Groups ++ [{R1, R2, R3}]).

search_repeated(C1, C2, C3)->
    search_repeated1(C1, C2, C3, []).

search_repeated1("", _, _, RC) ->
    RC;
search_repeated1([E1 | Rest], C2, C3, RC) ->
    InCompartment2 = lists:member(E1, C2),
    InCompartment3 = lists:member(E1, C3),
    NotInResult = not lists:member(E1, RC),
    if
        InCompartment2 and NotInResult and InCompartment3 ->
            search_repeated1(Rest, C2, C3, RC ++ [E1]);
        true ->
            search_repeated1(Rest, C2, C3, RC)
    end.

