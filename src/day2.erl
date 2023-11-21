-module(day2).

-export([puzzle1/1, puzzle2/1]).

%%==============================================================================
%% Day 2 Puzzle 1
%%==============================================================================
-spec puzzle1(string()) -> integer().
puzzle1(FileName) ->
    Guide = read_guide(FileName),
    p1_sum_rounds(Guide).

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

p1_round("A","X") ->
    3;
p1_round("B", "Y") ->
    3;
p1_round("C", "Z") ->
    3;
p1_round("A","Z") ->
    0;
p1_round("B", "X") ->
    0;
p1_round("C", "Y") ->
    0;
p1_round("A","Y") ->
    6;
p1_round("B", "Z") ->
    6;
p1_round("C", "X") ->
    6.

p1_calculate_round([A, B]) ->
    own_selection(B) + p1_round(A, B).

p1_sum_rounds(Rounds) ->
    lists:foldl(fun(Round, Sum) ->
            p1_calculate_round(Round) + Sum
        end, 0, Rounds).

%%==============================================================================
%% Day 2 Puzzle 2
%%==============================================================================
-spec puzzle2(string()) -> integer().
puzzle2(FileName) ->
    Guide = read_guide(FileName),
    p2_sum_rounds(Guide).

% A = Rock,1     X = Lose,0
% B = Paper,2    Y = Draw,3
% C = Scissors,3 Z = Win,6
p2_round("A","X") ->
    {0, 3};
p2_round("B", "Y") ->
    {3, 2};
p2_round("C", "Z") ->
    {6, 1};
p2_round("A","Z") ->
    {6, 2};
p2_round("B", "X") ->
    {0, 1};
p2_round("C", "Y") ->
    {3, 3};
p2_round("A","Y") ->
    {3, 1};
p2_round("B", "Z") ->
    {6, 3};
p2_round("C", "X") ->
    {0, 2}.

p2_calculate_round([A, B]) ->
    {Result, Selection} = p2_round(A, B),
    Result + Selection.

p2_sum_rounds(Rounds) ->
    lists:foldl(fun(Round, Sum) ->
            p2_calculate_round(Round) + Sum
        end, 0, Rounds).
