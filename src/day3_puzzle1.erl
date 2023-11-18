-module(day3_puzzle1).

-export([main/1, calculate_points/1]).

main(FileName) ->
    Rucksacks = read_rucksacks(FileName),
    RepeatedItems = lists:map(
        fun({C1, C2}) ->
            search_repeated(C1, C2)
        end, Rucksacks),
    calculate_points(RepeatedItems).

read_rucksacks(FileName) ->
    {ok, RawInput} = file:read_file(FileName),
    RawData = binary:bin_to_list(RawInput),
    RucksacksData = string:split(RawData, "\n", all),
    RucksacksItems = lists:map(
        fun(Compartments) ->
            {string:substr(Compartments, 1, trunc(length(Compartments)/2)),
                string:substr(Compartments, trunc(length(Compartments)/2)+1, length(Compartments))}
        end, RucksacksData),
    RucksacksItems.

search_repeated(C1, C2)->
    search_repeated1(C1, C2, []).

search_repeated1("", _, RC) ->
    RC;
search_repeated1([E1 | Rest], C2, RC) ->
    InCompartment2 = lists:member(E1, C2),
    NotInResult = not lists:member(E1, RC),
    if
        InCompartment2 and NotInResult ->
            search_repeated1(Rest, C2, RC ++ [E1]);
        true ->
            search_repeated1(Rest, C2, RC)
    end.

calculate_points(Letters) ->
    lists:sum(
        lists:map(
            fun(L) ->
                calculate_point_min(L) + calculate_point_may(L)
            end, Letters)).

calculate_point_min([L | _])->
    Mins = "abcdefghijklmnopqrstuvwxyz",
    IsMin = lists:member(L, Mins),
    if
        IsMin ->
            string:str(Mins, [L]);
        true ->
            0
    end.
calculate_point_may([L | _])->
    Mays = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    IsMay = lists:member(L, Mays),
    if
        IsMay ->
            string:str(Mays, [L])  + 26;
        true ->
            0
    end.
