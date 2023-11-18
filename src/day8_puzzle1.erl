-module(day8_puzzle1).
-export([main/1, read_stream/1]).

main(FileName) ->
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

%visible_element(_, []) -> true;
%visible_element(Elem, [H | _Rest]) when H >= Elem -> false;
%visible_element(Elem, [_H | Rest]) -> visible_element(Elem, Rest).