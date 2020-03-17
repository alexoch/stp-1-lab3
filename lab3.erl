%%%%%%%%%%%     erl +pc unicode
%%%%%%%%%%%     mp-posts_full_1.csv  map_zal-skl9_1.csv plenary_register_mps-skl9_1.tsv
%%%%%%%%%%%     plenary_vote_results-skl9_2.tsv
%%%%%%%%%%%     mps-declarations_rada_3.csv

-module(lab3).
-export([load/2, stringMod/1, outputFile/1, cmd/0, getSeparator/1, formatRowOutput/1, formatTable/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% main func

cmd() -> 
    {ok, Command, EndLoc} = io:scan_erl_exprs('Enter command>'),
    case lists:nth(3, tuple_to_list(lists:nth(1, Command))) of
        load -> loadController(Command)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% load function

load(Path, Sep) ->
    {ok, Text} = file:read_file(Path),
    ListText = unicode:characters_to_list(Text, utf8),
    AfterFirstSplit = string:split(ListText, "\n", all),
    if
        (Sep == ",") -> AfterSecondSplit = lists:map(fun(List) -> re:split(stringMod(List), ",,", [unicode]) end, AfterFirstSplit);
        true ->  AfterSecondSplit = lists:map(fun(List) -> re:split(List, Sep, [unicode]) end, AfterFirstSplit)
    end,
    lists:map(
        fun(List) -> 
            lists:map(
                fun(WrappedList) -> 
                    unicode:characters_to_list(WrappedList)
                end,
                List
            )
        end,
        AfterSecondSplit
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% load command controller

loadController(Command) -> 
    ISValid = (lists:nth(1, tuple_to_list(lists:nth(2, Command))) == '(') and 
        (lists:nth(1, tuple_to_list(lists:nth(3, Command))) == string) and
        (lists:nth(1, tuple_to_list(lists:nth(4, Command))) == ')') and
        (lists:nth(1, tuple_to_list(lists:nth(5, Command))) == dot),
    if
        ISValid -> outputFile(lists:nth(3, tuple_to_list(lists:nth(3, Command))));
        true -> "Invalid input!"
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  String modification function

stringMod([]) -> [];
stringMod(Str) -> 
    [H|T] = string:split(Str, ","),
    if
        T /= [] ->
            RealTail = lists:nth(1, T),
            if RealTail /= [] ->
                First = lists:nth(1, RealTail),
                case First /= 32 of
                    true -> string:concat(string:concat(H,",,"), stringMod(T));
                    false -> string:concat(string:concat(H,","), stringMod(T))
                end;
                true -> H
            end;
        true -> H
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% separator getter from file format

getSeparator(FileName) -> 
    Format = string:sub_string(FileName, string:length(FileName) - 3),
    case Format of
        ".csv" -> ",";
        ".tsv" -> "\t";
        _Else -> " "
    end.

outputFile(FileName) -> 
    Sep = getSeparator(FileName),  
    formatTable(load(FileName, Sep)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Format Row output

formatRowOutput([]) ->   io:fwrite("~n");
formatRowOutput([H|T]) -> 
    HLength =  string:length(H),
    if 
        (HLength > 19) -> io:fwrite("|~-30ts", [H]);
        (HLength > 15) -> io:fwrite("|~-20ts", [H]);
        (HLength > 8) -> io:fwrite("|~-15ts", [H]);
        true -> io:fwrite("|~-8ts", [H])
    end,
    formatRowOutput(T).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Format table out

formatTable([]) -> [];
formatTable([H|T]) -> 
    formatRowOutput(H),
    formatTable(T).