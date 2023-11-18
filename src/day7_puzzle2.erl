-module(day7_puzzle2).

-export([main/1]).

main(FileName)->
    History = puzzle1:read_stream(FileName),
    ListFilesAndDirs = puzzle1:list_files_and_dirs(History),
    FilesInDirs = puzzle1:find_files_in_dirs(ListFilesAndDirs),
    FolderSizes = puzzle1:get_folders_sizes(FilesInDirs),
    [{_, UsedSpace}] = lists:filter(fun({["/"], _}) -> true;
                                    (_) -> false end, FolderSizes),
    RequiredSpace = 30000000 - (70000000 - UsedSpace),
    DirWithRequiredSpace = lists:filter(fun({_, Size}) -> Size >= RequiredSpace end, FolderSizes),
    [{_, Result} | _] = lists:sort(fun({_, SizeA}, {_, SizeB}) -> SizeA < SizeB end, DirWithRequiredSpace),
    Result.

