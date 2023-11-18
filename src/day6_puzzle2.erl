-module(day6_puzzle2).

-export([main/1]).

main(FileName) ->
    Stream = puzzle1:read_stream(FileName),
    start_of_packet(Stream).

start_of_packet(Stream) ->
    start_of_packet(Stream, [], 0).
start_of_packet([H|T], Packet, Index) when length(Packet) < 14 ->
    start_of_packet(T, Packet ++ [H], Index + 1);
start_of_packet([H|T], Packet, Index) ->
    Duplicates = duplicates(Packet),
    if
        Duplicates ->
            [_ | NewPacket] = Packet,
            start_of_packet(T, NewPacket ++ [H], Index +1);
        true ->
            {Packet, Index}
    end.

duplicates(Packet) ->
    duplicates(Packet, []).
duplicates([], _) ->
    false;
duplicates([E1 | Packet1], Packet2) ->
    Member1 = lists:member(E1, Packet1),
    Member2 = lists:member(E1, Packet2),
    if
        Member1 or Member2 -> true;
        true -> duplicates(Packet1, Packet2 ++ [E1])
    end.

