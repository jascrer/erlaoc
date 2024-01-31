-module(day9).

-export([puzzle1/1, read_inst/1, move_head/2, move_tail/3, euclidean/2, puzzle2/1]).

%%==============================================================================
%% Day 9 Puzzle 1
%%==============================================================================
-spec puzzle1(string()) -> integer().
puzzle1(FileName) ->
    Instr = read_inst(FileName),
    simulate(Instr).

read_inst(FileName) ->
    {ok, RawInput} = file:read_file(FileName),
    RawData = binary:bin_to_list(RawInput),
    lists:map(
        fun(Elem) ->
            [Direc, Pos] = string:split(Elem, " ", all),
            {IntPos, []} = string:to_integer(Pos),
            {Direc, IntPos}
        end, string:split(RawData, "\n", all)).

get_start() -> {0,4}.

euclidean({X1, Y1}, {X2, Y2}) ->
    trunc(math:pow(X2 - X1,2)) + trunc(math:pow(Y2-Y1,2)).

move_head(Dir, {X, Y}) ->
    case Dir of
        "U" -> {X, Y - 1};
        "D" -> {X, Y + 1};
        "L" -> {X - 1, Y};
        "R" -> {X + 1, Y}
    end.

move_tail(Dist, H, _T) when Dist > 2 ->
    H;
move_tail(_, _H, T) ->
    T.

move_pair(Dir, H, T) ->
    NHead = move_head(Dir, H),
    Dist = euclidean(NHead, T),
    NTail = move_tail(Dist, H, T),
    {NHead, NTail}.

execute_inst({_Dir,0}, H, T, Pos) ->
    {H, T, Pos};
execute_inst({Dir,Spaces}, H, T, Pos) ->
    {NHead, NTail} = move_pair(Dir, H, T),
    execute_inst({Dir, Spaces - 1}, NHead, NTail, sets:add_element(NTail, Pos)).

simulate(Instructions)->
    Head = get_start(),
    Tail = get_start(),
    simulate(Instructions, Head, Tail, sets:new()).
simulate([], _Head, _Tail, TPos) ->
    sets:size(TPos);
simulate([Instr | Rest], Head, Tail, TPos) ->
    {NHead, NTail, NTPos} = execute_inst(Instr, Head, Tail, TPos),
    simulate(Rest, NHead, NTail, NTPos).

%%==============================================================================
%% Day 9 Puzzle 2
%%==============================================================================
-spec puzzle2(string()) -> integer().
puzzle2(FileName) ->
    Instr = read_inst(FileName),
    p2_simulate(Instr).

p2_simulate(Instructions) ->
    Rope = [{11,15} || _ <- lists:seq(1,10)],
    p2_simulate(Instructions, Rope, sets:new()).
p2_simulate([], _Rope, Pos) -> sets:size(Pos);
p2_simulate([Inst | Rest], Rope, Pos) ->
    {NRope, NPos} = p2_execute_inst(Inst, Rope, Pos),
    p2_simulate(Rest, NRope, NPos).


p2_execute_inst({_Dir,0}, Rope, Pos)-> {Rope, Pos};
p2_execute_inst({Dir, Spaces}, Rope, Pos) ->
    {NRope, NPos} = move_rope(Dir, Rope, Pos),
    p2_execute_inst({Dir, Spaces - 1}, NRope, NPos).

move_rope(Dir, [H | Rest], Pos) ->
    NHead = move_head(Dir, H),
    move_rope2(Rest, Pos, [NHead]).
move_rope2([], Pos, [New | Rope]) ->
    {lists:reverse([New | Rope]), sets:add_element(New, Pos)};
move_rope2([H | Rest], Pos, Rope) ->
    [Prev | _] = Rope,
    New = p2_move_tail(Prev, H),
    move_rope2(Rest, Pos, [New | Rope]).

p2_move_tail(Prev, Node) ->
    Dist = euclidean(Prev, Node),
    p2_move_tail(Dist, Prev, Node).
p2_move_tail(Dist, Prev, Node) when Dist == 4 ->
    get_cruz(Prev, Node);
p2_move_tail(Dist, Prev, {X,Y}) when Dist > 4 ->
    get_diagonal(Prev, {X,Y});
p2_move_tail(_Dist, _Prev, Node) ->
    Node.

get_cruz({PX, PY}, {X, Y}) when (PX == X - 2) and (PY == Y)->
    {X - 1, Y};
get_cruz({PX, PY}, {X, Y}) when (PX == X) and (PY == Y - 2)->
    {X, Y - 1};
get_cruz({PX, PY}, {X, Y}) when (PX == X) and (PY == Y + 2)->
    {X, Y + 1};
get_cruz({PX, PY}, {X, Y}) when (PX == X + 2) and (PY == Y)->
    {X + 1, Y}.

get_diagonal({PX, PY}, {X,Y}) when (PX == X - 2) and (PY == Y - 1) ->
    {X - 1, Y - 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X - 2) and (PY == Y + 1) ->
    {X - 1, Y + 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X + 2) and (PY == Y - 1) ->
    {X + 1, Y - 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X + 2) and (PY == Y + 1) ->
    {X + 1, Y + 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X - 1) and (PY == Y - 2) ->
    {X - 1, Y - 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X + 1) and (PY == Y - 2) ->
    {X + 1, Y - 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X - 1) and (PY == Y + 2) ->
    {X - 1, Y + 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X + 1) and (PY == Y + 2) ->
    {X + 1, Y + 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X - 2) and (PY == Y - 2) ->
    {X - 1, Y - 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X - 2) and (PY == Y + 2) ->
    {X - 1, Y + 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X + 2) and (PY == Y - 2) ->
    {X + 1, Y - 1};
get_diagonal({PX, PY}, {X,Y}) when (PX == X + 2) and (PY == Y + 2) ->
    {X + 1, Y + 1}.
