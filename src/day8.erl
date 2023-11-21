-module(day8).
-export([puzzle1/1, puzzle2/1]).

puzzle1(FileName) ->
    Board = read_stream(FileName),
    VisBoard = eval_positions(Board),
    lists:sum(lists:map(
                fun(List) ->
                    length(lists:filter(fun({_, Vis}) -> Vis end, List))
                end, VisBoard)).


read_stream(FileName) ->
    {ok, RawInput} = file:read_file(FileName),
    RawData = binary:bin_to_list(RawInput),
    lists:map(
        fun(Digits) ->
            lists:map(
                fun(Digit) ->
                    {CDigit, []} = string:to_integer("" ++ [Digit]),
                    CDigit
                end, Digits)
        end, string:split(RawData, "\n", all)).

eval_positions(List)->
    eval_positions(List, 1, []).
eval_positions(List, Row, Acc) when Row == length(List) +1 ->
    Acc;
eval_positions(List, Row, Acc) when Row == 1 ->
    VisibleElem = lists:map(
        fun(Elem) ->
            {Elem, true}
        end, lists:nth(Row, List)),
    eval_positions(List, Row + 1, Acc ++ [VisibleElem]);
eval_positions(List, Row, Acc) when length(List) == Row ->
    VisibleElem = lists:map(
        fun(Elem) ->
            {Elem, true}
        end, lists:nth(Row, List)),
    eval_positions(List, Row + 1, Acc ++ [VisibleElem]);
eval_positions(List, Row, Acc) ->
    Elements = eval_row(List, lists:nth(Row, List), Row, 1, []),
    eval_positions(List, Row + 1, Acc ++ [Elements]).

eval_row(_List, ActualList, _Row, Col, Acc) when Col == length(ActualList) + 1 ->
    Acc;
eval_row(List, ActualList, Row, Col, Acc) when Col == 1 ->
    eval_row(List, ActualList, Row, Col + 1, Acc ++ [{lists:nth(Col, ActualList),  true}]);
eval_row(List, ActualList, Row, Col, Acc) when length(ActualList) == Col ->
    eval_row(List, ActualList, Row, Col + 1, Acc ++ [{lists:nth(Col, ActualList),  true}]);
eval_row(List, ActualList, Row, Col, Acc) ->
    Right = right_vis(Col, ActualList),
    Left = left_vis(Col, ActualList),
    Top = top_vis(List, ActualList, Row, Col),
    Bot = bot_vis(List, ActualList, Row, Col),
    Visibility = Right or Left or Top or Bot,
    eval_row(List, ActualList, Row, Col + 1, Acc ++ [{lists:nth(Col, ActualList), Visibility}]).

right_vis(Pos, ActualList) ->
    right_vis(Pos, Pos + 1 , ActualList).
right_vis(_Pos, Next, ActualList) when Next == length(ActualList) + 1 ->
    true;
right_vis(Pos, Next, ActualList) ->
    ActualEl = lists:nth(Pos, ActualList),
    NextEl = lists:nth(Next, ActualList),
    if
        ActualEl =< NextEl ->
            false;
        true ->
            right_vis(Pos, Next + 1, ActualList)
    end.

left_vis(Pos, ActualList) ->
    left_vis(Pos, 1, ActualList).
left_vis(Pos, Before, _ActualList) when Before == Pos ->
    true;
left_vis(Pos, Before, ActualList) ->
    ActualEl = lists:nth(Pos, ActualList),
    BeforeEl = lists:nth(Before, ActualList),
    if
        ActualEl =< BeforeEl ->
            false;
        true ->
            left_vis(Pos, Before + 1, ActualList)
    end.

top_vis(List, ActualList, Row, Col) ->
    top_vis(List, ActualList, Row, Col, 1).
top_vis(_List, _ActualList, Row, _Col, Nav) when Nav == Row -> 
    true;
top_vis(List, ActualList, Row, Col, Nav) ->
    Actual = lists:nth(Col, ActualList),
    Top = lists:nth(Col, lists:nth(Nav, List)),
    if
        Actual =< Top ->
            false;
        true ->
            top_vis(List, ActualList, Row, Col, Nav + 1)
    end.

bot_vis(List, ActualList, Row, Col) ->
    bot_vis(List, ActualList, Row, Col, Row+1).
bot_vis(List, _ActualList, _Row, _Col, Nav) when Nav == length(List) + 1 -> 
    true;
bot_vis(List, ActualList, Row, Col, Nav) ->
    Actual = lists:nth(Col, ActualList),
    Bot = lists:nth(Col, lists:nth(Nav, List)),
    if
        Actual =< Bot ->
            false;
        true ->
            bot_vis(List, ActualList, Row, Col, Nav + 1)
    end.

puzzle2(FileName) ->
    Board = read_stream(FileName),
    VisBoard = p2_eval_positions(Board),
    [H | _] = lists:sort(fun({_, TreeA}, {_, TreeB}) -> TreeA >= TreeB end, 
        lists:flatten(VisBoard)),
    H.

p2_eval_positions(List)->
    p2_eval_positions(List, 1, []).
p2_eval_positions(List, Row, Acc) when Row == length(List) +1 ->
    Acc;
p2_eval_positions(List, Row, Acc) when Row == 1 ->
    VisibleElem = lists:map(
        fun(Elem) ->
            {Elem, 1}
        end, lists:nth(Row, List)),
    p2_eval_positions(List, Row + 1, Acc ++ [VisibleElem]);
p2_eval_positions(List, Row, Acc) when length(List) == Row ->
    VisibleElem = lists:map(
        fun(Elem) ->
            {Elem, 1}
        end, lists:nth(Row, List)),
    p2_eval_positions(List, Row + 1, Acc ++ [VisibleElem]);
p2_eval_positions(List, Row, Acc) ->
    Elements = p2_eval_row(List, lists:nth(Row, List), Row, 1, []),
    p2_eval_positions(List, Row + 1, Acc ++ [Elements]).

p2_eval_row(_List, ActualList, _Row, Col, Acc) when Col == length(ActualList) + 1 ->
    Acc;
p2_eval_row(List, ActualList, Row, Col, Acc) when Col == 1 ->
    p2_eval_row(List, ActualList, Row, Col + 1, Acc ++ [{lists:nth(Col, ActualList),  1}]);
p2_eval_row(List, ActualList, Row, Col, Acc) when length(ActualList) == Col ->
    p2_eval_row(List, ActualList, Row, Col + 1, Acc ++ [{lists:nth(Col, ActualList),  1}]);
p2_eval_row(List, ActualList, Row, Col, Acc) ->
    Right = p2_right_vis(Col, ActualList),
    Left = p2_left_vis(Col, ActualList),
    Top = p2_top_vis(List, ActualList, Row, Col),
    Bot = p2_bot_vis(List, ActualList, Row, Col),
    Visibility = Right * Left * Top * Bot,
    p2_eval_row(List, ActualList, Row, Col + 1, Acc ++ [{lists:nth(Col, ActualList), Visibility}]).

p2_right_vis(Pos, ActualList) ->
    p2_right_vis(Pos, Pos + 1 , ActualList).
p2_right_vis(Pos, Next, ActualList) when Next == length(ActualList) + 1 ->
    Next - Pos - 1;
p2_right_vis(Pos, Next, ActualList) ->
    ActualEl = lists:nth(Pos, ActualList),
    NextEl = lists:nth(Next, ActualList),
    if
        ActualEl =< NextEl ->
            Next - Pos;
        true ->
            p2_right_vis(Pos, Next + 1, ActualList)
    end.
    
p2_left_vis(Pos, ActualList) ->
    p2_left_vis(Pos, Pos - 1, ActualList).
p2_left_vis(Pos, Before, _ActualList) when Before == 0 ->
    Pos - 1;
p2_left_vis(Pos, Before, ActualList) ->
    ActualEl = lists:nth(Pos, ActualList),
    BeforeEl = lists:nth(Before, ActualList),
    if
        ActualEl =< BeforeEl ->
            Pos - Before;
        true ->
            p2_left_vis(Pos, Before - 1, ActualList)
    end.

p2_top_vis(List, ActualList, Row, Col) ->
    p2_top_vis(List, ActualList, Row, Col, Row - 1).
p2_top_vis(_List, _ActualList, Row, _Col, Nav) when Nav == 0 -> 
    Row - 1;
p2_top_vis(List, ActualList, Row, Col, Nav) ->
    Actual = lists:nth(Col, ActualList),
    Top = lists:nth(Col, lists:nth(Nav, List)),
    if
        Actual =< Top ->
            Row - Nav;
        true ->
            p2_top_vis(List, ActualList, Row, Col, Nav - 1)
    end.
    
p2_bot_vis(List, ActualList, Row, Col) ->
    p2_bot_vis(List, ActualList, Row, Col, Row+1).
p2_bot_vis(List, _ActualList, Row, _Col, Nav) when Nav == length(List) + 1 -> 
    Nav - Row - 1;
p2_bot_vis(List, ActualList, Row, Col, Nav) ->
    Actual = lists:nth(Col, ActualList),
    Bot = lists:nth(Col, lists:nth(Nav, List)),
    if
        Actual =< Bot ->
            Nav - Row;
        true ->
            p2_bot_vis(List, ActualList, Row, Col, Nav + 1)
    end.