-module(day2_puzzle2).

-export([main/1]).


main(FileName) ->
    Guide = puzzle1:read_guide(FileName),
    sum_rounds(Guide).

% A = Rock,1     X = Lose,0
% B = Paper,2    Y = Draw,3
% C = Scissors,3 Z = Win,6
round("A","X") ->
    {0, 3};
round("B", "Y") ->
    {3, 2};
round("C", "Z") ->
    {6, 1};
round("A","Z") ->
    {6, 2};
round("B", "X") ->
    {0, 1};
round("C", "Y") ->
    {3, 3};
round("A","Y") ->
    {3, 1};
round("B", "Z") ->
    {6, 3};
round("C", "X") ->
    {0, 2}.

calculate_round([A, B]) ->
    {Result, Selection} = round(A, B),
    Result + Selection.

sum_rounds(Rounds) ->
    lists:foldl(fun(Round, Sum) ->
            calculate_round(Round) + Sum
        end, 0, Rounds).