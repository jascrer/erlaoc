-module(day8_puzzle2).

-export([main/1]).

main(FileName) ->
    Board = puzzle1:read_stream(FileName),
    VisBoard = eval_positions(Board),
    [H | _] = lists:sort(fun({_, TreeA}, {_, TreeB}) -> TreeA >= TreeB end, 
        lists:flatten(VisBoard)),
    H.

eval_positions(List)->
    eval_positions(List, 1, []).
eval_positions(List, Row, Acc) when Row == length(List) +1 ->
    Acc;
eval_positions(List, Row, Acc) when Row == 1 ->
    VisibleElem = lists:map(
        fun(Elem) ->
            {Elem, 1}
        end, lists:nth(Row, List)),
    eval_positions(List, Row + 1, Acc ++ [VisibleElem]);
eval_positions(List, Row, Acc) when length(List) == Row ->
    VisibleElem = lists:map(
        fun(Elem) ->
            {Elem, 1}
        end, lists:nth(Row, List)),
    eval_positions(List, Row + 1, Acc ++ [VisibleElem]);
eval_positions(List, Row, Acc) ->
    Elements = eval_row(List, lists:nth(Row, List), Row, 1, []),
    eval_positions(List, Row + 1, Acc ++ [Elements]).

eval_row(_List, ActualList, _Row, Col, Acc) when Col == length(ActualList) + 1 ->
    Acc;
eval_row(List, ActualList, Row, Col, Acc) when Col == 1 ->
    eval_row(List, ActualList, Row, Col + 1, Acc ++ [{lists:nth(Col, ActualList),  1}]);
eval_row(List, ActualList, Row, Col, Acc) when length(ActualList) == Col ->
    eval_row(List, ActualList, Row, Col + 1, Acc ++ [{lists:nth(Col, ActualList),  1}]);
eval_row(List, ActualList, Row, Col, Acc) ->
    Right = right_vis(Col, ActualList),
    Left = left_vis(Col, ActualList),
    Top = top_vis(List, ActualList, Row, Col),
    Bot = bot_vis(List, ActualList, Row, Col),
    %io:format("Actual ~p Right ~p Left ~p Top ~p Bot ~p~n", [lists:nth(Col, ActualList), Right, Left, Top, Bot]),
    Visibility = Right * Left * Top * Bot,
    eval_row(List, ActualList, Row, Col + 1, Acc ++ [{lists:nth(Col, ActualList), Visibility}]).

right_vis(Pos, ActualList) ->
    right_vis(Pos, Pos + 1 , ActualList).
right_vis(Pos, Next, ActualList) when Next == length(ActualList) + 1 ->
    Next - Pos - 1;
right_vis(Pos, Next, ActualList) ->
    ActualEl = lists:nth(Pos, ActualList),
    NextEl = lists:nth(Next, ActualList),
    if
        ActualEl =< NextEl ->
            Next - Pos;
        true ->
            right_vis(Pos, Next + 1, ActualList)
    end.
    
left_vis(Pos, ActualList) ->
    left_vis(Pos, Pos - 1, ActualList).
left_vis(Pos, Before, _ActualList) when Before == 0 ->
    Pos - 1;
left_vis(Pos, Before, ActualList) ->
    ActualEl = lists:nth(Pos, ActualList),
    BeforeEl = lists:nth(Before, ActualList),
    if
        ActualEl =< BeforeEl ->
            Pos - Before;
        true ->
            left_vis(Pos, Before - 1, ActualList)
    end.

top_vis(List, ActualList, Row, Col) ->
    top_vis(List, ActualList, Row, Col, Row - 1).
top_vis(_List, _ActualList, Row, _Col, Nav) when Nav == 0 -> 
    Row - 1;
top_vis(List, ActualList, Row, Col, Nav) ->
    Actual = lists:nth(Col, ActualList),
    Top = lists:nth(Col, lists:nth(Nav, List)),
    if
        Actual =< Top ->
            Row - Nav;
        true ->
            top_vis(List, ActualList, Row, Col, Nav - 1)
    end.
    
bot_vis(List, ActualList, Row, Col) ->
    bot_vis(List, ActualList, Row, Col, Row+1).
bot_vis(List, _ActualList, Row, _Col, Nav) when Nav == length(List) + 1 -> 
    Nav - Row - 1;
bot_vis(List, ActualList, Row, Col, Nav) ->
    Actual = lists:nth(Col, ActualList),
    Bot = lists:nth(Col, lists:nth(Nav, List)),
    if
        Actual =< Bot ->
            Nav - Row;
        true ->
            bot_vis(List, ActualList, Row, Col, Nav + 1)
    end.