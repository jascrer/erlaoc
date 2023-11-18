-module(day2_puzzle1).

-export([main/1, read_guide/1]).


main(FileName) ->
    Guide = read_guide(FileName),
    sum_rounds(Guide).

read_guide(FileName) ->
    {ok, RawInput} = file:read_file(FileName),
    RawData = binary:bin_to_list(RawInput),
    RoundsData = string:split(RawData, "\n", all),
    RoundPlays = lists:map(
        fun(RoundData) ->
            string:split(RoundData, " ", all)
        end, RoundsData),
    RoundPlays.

own_selection("X") ->
    1;
own_selection("Y") ->
    2;
own_selection("Z") ->
    3.

round("A","X") ->
    3;
round("B", "Y") ->
    3;
round("C", "Z") ->
    3;
round("A","Z") ->
    0;
round("B", "X") ->
    0;
round("C", "Y") ->
    0;
round("A","Y") ->
    6;
round("B", "Z") ->
    6;
round("C", "X") ->
    6.

calculate_round([A, B]) ->
    own_selection(B) + round(A, B).

sum_rounds(Rounds) ->
    lists:foldl(fun(Round, Sum) ->
            calculate_round(Round) + Sum
        end, 0, Rounds).
