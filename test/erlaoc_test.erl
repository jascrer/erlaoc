-module(erlaoc_test).

-include_lib("eunit/include/eunit.hrl").

day1_test() ->
    %% Puzzle 1
    ?assertEqual(24000, day1:puzzle1("test/test_files/day1_test.txt")),
    %% Puzzle 2
    ?assertEqual(45000, day1:puzzle2("test/test_files/day1_test.txt")).

day2_test() ->
    %% Puzzle 1
    ?assertEqual(15, day2:puzzle1("test/test_files/day2_test.txt")),
    %% Puzzle 2
    ?assertEqual(12, day2:puzzle2("test/test_files/day2_test.txt")).

day3_test() ->
    %% Puzzle 1
    ?assertEqual(157, day3:puzzle1("test/test_files/day3_test.txt")),
    %% Puzzle 2
    ?assertEqual(70, day3:puzzle2("test/test_files/day3_test.txt")).

day4_test() ->
    %% Puzzle 1
    ?assertEqual(2, day4:puzzle1("test/test_files/day4_test.txt")),
    %% Puzzle 2
    ?assertEqual(4, day4:puzzle2("test/test_files/day4_test.txt")).

day5_test() ->
    %% Puzzle 1
    ?assertEqual([{1,["[C]"]},{2,["[M]"]},{3,["[Z]","[N]","[D]","[P]"]}], day5:puzzle1("test/test_files/day5_test.txt")),
    %% Puzzle 2
    ?assertEqual([{1,["[M]"]},{2,["[C]"]},{3,["[D]","[N]","[Z]","[P]"]}], day5:puzzle2("test/test_files/day5_test.txt")).

day6_test() ->
    %% Puzzle 1
    ?assertEqual({"zqfr",11}, day6:puzzle1("test/test_files/day6_test.txt")),
    %% Puzzle 2
    ?assertEqual({"jwzlrfnpqdbhtm",26}, day6:puzzle2("test/test_files/day6_test.txt")).

day7_test() ->
    %% Puzzle 1
    ?assertEqual(95437, day7:puzzle1("test/test_files/day7_test.txt")),
    %% Puzzle 2
    ?assertEqual(24933642, day7:puzzle2("test/test_files/day7_test.txt")).

day8_test() ->
    %% Puzzle 1
    ?assertEqual(21, day8:puzzle1("test/test_files/day8_test.txt")),
    %% Puzzle 2
    ?assertEqual({5,8}, day8:puzzle2("test/test_files/day8_test.txt")).

day9_test() ->
    %% Puzzle 1
    ?assertEqual(13, day9:puzzle1("test/test_files/day9_test.txt")),
    %% Puzzle 2
    ?assertEqual(36, day9:puzzle2("test/test_files/day9_test2.txt")).