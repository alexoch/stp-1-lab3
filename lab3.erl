-module(lab3).
-export([run/0]).

run() ->
    run(true).

run(true) ->
      {ok, [Command]} = io:fread("Enter a command: ","~s"),
      CommandName = commandName(Command),   
      FileName = fileName(Command),
      {ok, File} = file:open(FileName,[read, {encoding, unicode}]),
      CountLine = choose_LineCount(FileName),
      RowsList = read_line(File, CountLine),
      file: close(File),
      Pattern = choose_Pattern(FileName),
      startParse(RowsList, Pattern).

choose_Pattern(FileName) ->
    FileFormat = string: find(FileName, "."),
    if 
        FileFormat == ".tsv" ->
            "\t";
        FileFormat == ".csv" -> 
            "(*UTF8),\\S";
        true -> []
    end.

choose_LineCount(FileName) ->
    FileFormat = string: find(FileName, "."),
    if 
        FileFormat == ".tsv" ->
            42;
        FileFormat == ".csv" -> 
            990;
        true -> 100
    end.

read_line(File, 0) -> [];
read_line(File, Num) ->
    case io:get_line(File, "") of
         eof -> [];
         Line -> 
            Row = re: replace(unicode: characters_to_binary(Line, unicode), ",,", ",,,,", [{return,binary}, global]),
            Row2 = re: replace(unicode: characters_to_binary(Row, unicode), "(*UTF8),\\S", ",&", [{return,binary}, global]),
            [Row2 | read_line(File, Num - 1)]
    end.

commandName(Str) ->
    Pos = string: str(Str, "("),
    CommandName = string: substr(Str, 1, Pos - 1).

fileName(Str) ->
    Pos = string: str(Str, "("),
    FileName = string: substr(Str, Pos + 2, string: length(Str) - Pos - 3).

startParse(RowsList, Pattern) ->
    Row = hd(RowsList),
    Str = re: split(Row, Pattern, [{return,list}]),
    ColNum = number_of_columns(Str,0),
    ColumnLength = trunc(143 / ColNum),
    parse(RowsList, ColNum, ColumnLength, Pattern).

parse([],ColNum, ColumnLength, Pattern) -> io:format("");
parse(RowsList, ColNum, ColumnLength, Pattern)->
    Row = hd(RowsList),
    Str = re: split(unicode: characters_to_binary(Row, unicode), Pattern, [{return,binary}]),
    consoleOutput(Str, ColumnLength, ColNum, ColNum, Pattern),
    parse(tl(RowsList), ColNum, ColumnLength, Pattern).

number_of_columns([], Num) -> Num;
number_of_columns(List, Num) ->
    number_of_columns(tl(List), Num + 1).

consoleOutput(Str, ColumnLength, 1,ColNumC, Pattern) ->
    Column = 143 - ColNumC * ColumnLength,
    if 
        Pattern == "\t" ->
            Str1 = Str;
        true -> 
            Str1 = string: substr(unicode: characters_to_list(Str), 1, ColumnLength + Column)
    end,
    io:fwrite([Str1]);
consoleOutput(Str, ColumnLength, ColNum, ColNumC, Pattern) -> 
    OutputPattern = "~-" ++ integer_to_list(ColumnLength) ++ "ts|",
    io:fwrite(OutputPattern, [hd(Str)]),
    consoleOutput(tl(Str), ColumnLength,ColNum - 1,ColNumC, Pattern).




