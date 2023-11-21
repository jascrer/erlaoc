-module(d9p2).
-import(day9_puzzle1, [read_inst/1, move_head/2, euclidean/2]).
-export([main/1]).

main(FileName) ->
    Instr = read_inst(FileName),
    simulate(Instr).


move_tail(Dir, Prev, Node) ->
    Dist = euclidean(Prev, Node),
    move_tail(Dir, Dist, Prev, Node).
move_tail(Dir, Dist, _Prev, Node) when Dist == 4 ->
    move_head(Dir, Node);
move_tail(Dir, Dist, Prev, {X,Y}) when Dist > 4 ->
    case Dir of
        "U" -> 
            Diag = {{X-1, Y-1}, {X+1, Y-1}},
            get_diagonal(Prev, Diag);
        "D" -> 
            Diag = {{X-1, Y+1}, {X+1, Y+1}},
            get_diagonal(Prev, Diag);
        "L" -> 
            Diag = {{X-1, Y-1}, {X-1, Y+1}},
            get_diagonal(Prev, Diag);
        "R" -> 
            Diag = {{X+1, Y-1}, {X+1, Y+1}},
            get_diagonal(Prev, Diag)
    end;
move_tail(_Dir, _Dist, _Prev, Node) ->
    Node.

get_diagonal(Prev, {Diag1, Diag2}) ->
    Eucl1 = euclidean(Prev, Diag1),
    if
        Eucl1 =< 2 ->
            Diag1;
        true ->
            Diag2
        end.

move_rope(Dir, [H | Rest], Pos) ->
    NHead = move_head(Dir, H),
    move_rope(Dir, Rest, Pos, [NHead]).
move_rope(_Dir, [], Pos, [New | Rope]) ->
    {lists:reverse([New | Rope]), sets:add_element(New, Pos)};
move_rope(Dir, [H | Rest], Pos, Rope) ->
    [Prev | _] = Rope,
    New = move_tail(Dir, Prev, H),
    move_rope(Dir, Rest, Pos, [New | Rope]).

execute_inst({_Dir,0}, Rope, Pos)->
    {Rope, Pos};
execute_inst({Dir, Spaces}, Rope, Pos) ->
    {NRope, NPos} = move_rope(Dir, Rope, Pos),
    execute_inst({Dir, Spaces - 1}, NRope, NPos).

simulate(Instructions) ->
    Rope = [{11,15} || _ <- lists:seq(1,10)],
    simulate(Instructions, Rope, sets:new()).
simulate([], _Rope, Pos) ->
    sets:size(Pos);
simulate([Inst | Rest], Rope, Pos) ->
    {NRope, NPos} = execute_inst(Inst, Rope, Pos),
    io:format("~p~n", [NRope]),
    simulate(Rest, NRope, NPos).