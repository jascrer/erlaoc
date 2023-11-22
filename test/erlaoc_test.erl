-module(erlaoc_test).

-include_lib("eunit/include/eunit.hrl").

day1_puzzle1_test() ->
    ?assertEqual(24000, day1:puzzle1("../test/test_files/day1_test.txt")).