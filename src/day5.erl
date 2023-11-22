-module(day5).

-export([puzzle1/1, puzzle2/1]).

%%==============================================================================
%% Day 5 Puzzle 1
%%==============================================================================
-spec puzzle1(string()) -> list().
puzzle1(FileName) ->
    {Tuples, Moves} = read_stacks(FileName),
    MovedTuples = move_crates(Tuples, Moves),
    lists:sort(
        fun({Index1, _}, {Index2, _}) ->
            Index1 =< Index2
        end, MovedTuples).

read_stacks(FileName) ->
    {ok, RawInput} = file:read_file(FileName),
    RawData = binary:bin_to_list(RawInput),
    [Stacks, Instruc] = string:split(RawData, "\n\n", all),
    {format_stacks(Stacks), format_instructions(Instruc)}.

format_instructions(RawInstructions) ->
    lists:map(
        fun(Instruction) ->
            [_, Quantity, _, Stack1, _, Stack2]=string:split(Instruction, " ", all),
            {IntQuantity, []} = string:to_integer(Quantity),
            {IntStack1, []} = string:to_integer(Stack1),
            {IntStack2, []} = string:to_integer(Stack2),
            {IntQuantity, IntStack1, IntStack2}
        end, string:split(RawInstructions, "\n", all)).

format_stacks(RawStacks) ->
    StackRowLists = lists:map(
        fun(Row) ->
            string:split(Row, " ", all)
        end, string:split(RawStacks, "\n", all)),
    CleanedStacks = lists:map(
        fun(RowList) ->
            format_row(RowList, [])
        end, StackRowLists),
    convert_to_tuples(CleanedStacks).

format_row([], Row) ->
    Row;
format_row([[],[],[]], Row) ->
    Row ++ [[]];
format_row([[],[],[],[] | Rest], Row) ->
    format_row(Rest, Row ++ [[]]);
format_row([Container | Rest], Row) ->
    format_row(Rest, Row ++ [Container]).

convert_to_tuples(Stacks) ->
    Indexes = lists:filter(
        fun(X) -> X /= [] end, lists:last(Stacks)),
    Tuples = lists:map(
        fun(Index) ->
            string:to_integer(Index)
        end, Indexes),
    separate_crates(Tuples, Stacks).

convert_to_tuples([], _) ->
    [];
convert_to_tuples([{Index, Row} | Rest],[Head | Tail]) ->
    [{Index, Row ++ [Head]}] ++ convert_to_tuples(Rest,Tail).

separate_crates(Tuples, Stack) when length(Stack) == 1 ->
    lists:foldl(fun clean_tuples/2, [], Tuples);
separate_crates(Tuples, [HeadStack | TailStack]) ->
    Result = convert_to_tuples(Tuples, HeadStack),
    separate_crates(Result, TailStack).

clean_tuples({Index, List}, Acc) -> 
    EmptySpaces = lists:foldl(fun clean_empty_spaces/2, [], List),
    Acc ++ [{Index, EmptySpaces}].

clean_empty_spaces([], List) -> List;
clean_empty_spaces(Elem, List) -> List ++ [Elem].

move_crates(Tuples, []) ->
    Tuples;
move_crates(Tuples, [{Quantity, Stack1, Stack2} | Moves]) ->
    ResultTuple = move_crate(Tuples, Quantity, Stack1, Stack2),
    move_crates(ResultTuple, Moves).

move_crate(Tuples, 0, _, _) ->
    Tuples;
move_crate(Tuples, Quantity, Stack1, Stack2) ->
    {Tuple1, Tuple2, OtherTuples} = extract_tuples(Tuples, {Stack1, Stack2}),
    [Crate, TupleFrom] = move_crate_from(Tuple1),
    TupleTo = move_crate_to(Tuple2, Crate),
    move_crate(OtherTuples ++ [TupleFrom] ++ [TupleTo], Quantity - 1, Stack1, Stack2).

move_crate_from({Index, [Crate | Rest]}) ->
    [Crate, {Index, Rest}].
move_crate_to({Index, Crates}, Crate) ->
    {Index, [Crate] ++ Crates}.

extract_tuples(Tuples, Stacks) ->
    extract_tuples(Tuples, Stacks, {}, {}, []).
extract_tuples([], _, Tuple1, Tuple2, Others) ->
    {Tuple1, Tuple2, Others};
extract_tuples([{Index, List} | Rest], {Stack1, Stack2}, _Tuple1, Tuple2, Others) when Index == Stack1 ->
    extract_tuples(Rest, {Stack1, Stack2}, {Index, List}, Tuple2, Others);
extract_tuples([{Index, List} | Rest], {Stack1, Stack2}, Tuple1, _Tuple2, Others) when Index == Stack2 ->
    extract_tuples(Rest, {Stack1, Stack2}, Tuple1, {Index, List}, Others);
extract_tuples([{Index, List} | Rest], {Stack1, Stack2}, Tuple1, Tuple2, Others) ->
    extract_tuples(Rest, {Stack1, Stack2}, Tuple1, Tuple2, Others ++ [{Index, List}]).

%%==============================================================================
%% Day 5 Puzzle 2
%%==============================================================================
-spec puzzle2(string()) -> list().
puzzle2(FileName) ->
    {Tuples, Moves} = read_stacks(FileName),
    MovedTuples = p2_move_crates(Tuples, Moves),
    lists:sort(
        fun({Index1, _}, {Index2, _}) ->
            Index1 =< Index2
        end, MovedTuples).

p2_move_crates(Tuples, []) ->
    Tuples;
p2_move_crates(Tuples, [{Quantity, Stack1, Stack2} | Moves]) ->
    ResultTuple = p2_move_crate(Tuples, Quantity, Stack1, Stack2),
    p2_move_crates(ResultTuple, Moves).

p2_move_crate(Tuples, 0, _, _) ->
    Tuples;
p2_move_crate(Tuples, Quantity, Stack1, Stack2) ->
    {Tuple1, Tuple2, OtherTuples} = extract_tuples(Tuples, {Stack1, Stack2}),
    [Crate, TupleFrom] = p2_move_crate_from(Tuple1, Quantity),
    TupleTo = p2_move_crate_to(Tuple2, Crate),
    OtherTuples ++ [TupleFrom] ++ [TupleTo].

p2_move_crate_from(Tuple, Quantity) ->
    p2_move_crate_from(Tuple, Quantity, []).
p2_move_crate_from(Tuple, 0, Acc)->
    [Acc, Tuple];
p2_move_crate_from({Index, [Crate | Rest]}, Quantity, Acc) ->
    p2_move_crate_from({Index, Rest}, Quantity - 1, Acc ++ [Crate]).

p2_move_crate_to({Index, Crates}, Crate) ->
    {Index, Crate ++ Crates}.
