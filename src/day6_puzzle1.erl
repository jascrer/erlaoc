-module(day6_puzzle1).

-export([main/1, read_stream/1]).

main(FileName) ->
    Stream = read_stream(FileName),
    start_of_packet(Stream).

read_stream(FileName) ->
    {ok, RawInput} = file:read_file(FileName),
    RawData = binary:bin_to_list(RawInput),
    RawData.

start_of_packet(Stream) ->
    start_of_packet(Stream, [], 0).
start_of_packet([H|T], Packet, Index) when length(Packet) < 4 ->
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

duplicates([E1, E2, E3, E4]) ->
    Member1 = lists:member(E1, [E2, E3, E4]),
    Member2 = lists:member(E2, [E1, E3, E4]),
    Member3 = lists:member(E3, [E1, E2, E4]),
    Member4 = lists:member(E4, [E1, E2, E3]),
    Member1 or Member2 or Member3 or Member4.

