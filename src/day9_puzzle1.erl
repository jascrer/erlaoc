-module(day9_puzzle1).

-export([main/1, read_inst/1, move_head/2, move_tail/3, euclidean/2]).

main(FileName) ->
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



