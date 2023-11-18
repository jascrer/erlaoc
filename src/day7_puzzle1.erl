-module(day7_puzzle1).

-export([main/1, read_stream/1, list_files_and_dirs/1, find_files_in_dirs/1, get_folders_sizes/1]).


main(FileName) ->
    History = read_stream(FileName),
    ListFilesAndDirs = list_files_and_dirs(History),
    FilesInDirs = find_files_in_dirs(ListFilesAndDirs),
    FolderSizes = get_folders_sizes(FilesInDirs),
    lists:sum([Size || {_, Size} <- FolderSizes, Size < 100000]).

read_stream(FileName) ->
    {ok, RawInput} = file:read_file(FileName),
    RawData = binary:bin_to_list(RawInput),
    _SeparatedLines = lists:map(fun parse_command/1 ,string:split(RawData, "\n$ ", all)).

parse_command("$ cd /") -> {cd , "/"};
parse_command([$c, $d, 32 | DirName]) -> {cd, DirName};
parse_command([$l, $s, 10 | RawFiles]) ->
    Files = lists:map(
        fun(Element) ->
            [First, Second] = string:split(Element, " ", all),
            case First of
                "dir" -> {dir, Second};
                SizeRaw ->
                    {Size, ""} = string:to_integer(SizeRaw),
                    {file, Second, Size}
            end
        end, string:split(RawFiles, "\n", all)),
    {ls, Files}.

list_files_and_dirs(ShellInstructions) ->
    {_, FilesAndDirs} = lists:foldl(fun execute_command/2, {[], [{dir, ["/"]}]}, ShellInstructions),
    lists:map(
        fun({dir, Path}) -> {dir, lists:reverse(Path)};
            ({file, Path, Size}) -> {file, lists:reverse(Path), Size}
        end, FilesAndDirs).

execute_command({cd, ".."}, {[_ | Path], FilesAndDirs}) ->
    {Path, FilesAndDirs};
execute_command({cd, DirName}, {Path, FilesAndDirs}) ->
    {[DirName | Path], FilesAndDirs};
execute_command({ls, CurrFilesAndDirs}, {Path, FilesAndDirs}) ->
    NewFilesAndDirs = lists:map(
        fun({dir, DirName}) -> {dir, [DirName | Path]};
           ({file, FileName, Size}) -> {file, [FileName | Path], Size}
        end, CurrFilesAndDirs),
    {Path, NewFilesAndDirs ++ FilesAndDirs}.

contains([], _) -> true;
contains([Elem | DirRest], [Elem | FileRest])->
    contains(DirRest, FileRest);
contains(_, _) -> false.

find_files_in_dirs(FilesAndDirs) ->
    DirsList = lists:filter(fun({dir,_}) -> true;
                               (_) -> false end, FilesAndDirs),
    lists:map(
        fun({dir, Path})->
            {Path,
                lists:filter(fun({dir, _})-> false;
                                ({file, FilePath, _Size}) ->
                                    contains(Path, FilePath)
                            end, FilesAndDirs)}
        end, DirsList).

get_folder_size({FolderName, Files}) ->
    {FolderName, lists:sum([Size || {file, _, Size} <- Files])}.

get_folders_sizes(DirsAndFiles) ->
    lists:map(fun get_folder_size/1, DirsAndFiles).