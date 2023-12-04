-module(day3).

-export([run/0]).

-record(position, {x :: pos_integer(), y :: pos_integer()}).

-spec part_one(Numbers :: [{#position{}, string()}], Symbols :: sets:set(#position{})) -> integer().
part_one(Numbers, Symbols) ->
    lists:sum(lists:filtermap(fun({#position{x = X, y = Y}, Number}) ->
        
    end)).

-spec part_two(Numbers :: [{#position{}, string()}], Symbols :: sets:set(#position{})) -> integer().
part_two(Numbers, Symbols) ->
    0.

-spec parse_board(string()) -> {[{#position{}, string()}], sets:set(#position{})}.
parse_board(Board) ->
    BoardLines = string:lexemes(Board, "\n"),
    {_, Numbers, Symbols} = lists:foldr(fun(Row, {Y, Numbers, Symbols}) ->
        {_, _, YNumbers, YSymbols} = lists:foldr(fun(ColChar, {X, CollectedNumber, XNumbers, XSymbols}) ->
                {NewCollectedNumber, NewNumbers, NewSymbols} = case ColChar of
                    $. -> case CollectedNumber of
                        none -> {none, XNumbers, XSymbols};
                        {some, StartX, NumberString} -> {none, XNumbers ++ [{#position{x = StartX, y = Y}, NumberString}], XSymbols}
                    end;
                    Char when (Char >= $0) and (Char =< $9) -> case CollectedNumber of
                        none -> {{some, X, [Char]}, XNumbers, XSymbols};
                        {some, StartX, NumberString} -> {{some, StartX, NumberString ++ [Char]}, XNumbers, XSymbols}
                    end;
                    _ -> case CollectedNumber of
                        none -> {none, XNumbers, sets:add_element(#position{x = X, y = Y}, XSymbols)};
                        {some, StartX, NumberString} -> {none, XNumbers ++ [{#position{x = StartX, y = Y}, NumberString}], sets:add_element(#position{x = X, y = Y}, XSymbols)}
                    end
                end,
                {X + 1, NewCollectedNumber, NewNumbers, NewSymbols}
            end, {0, none, Numbers, Symbols}, Row),
        {Y - 1, YNumbers, YSymbols}
    end, {length(BoardLines), [], sets:new()}, BoardLines),
    {Numbers, Symbols}.


-spec run() -> ok | {error, string()}.
run() ->
    case file:read_file("input/day3.txt") of
        {ok, Input} ->
            InputString = binary_to_list(Input),
            {Numbers, Symbols} = parse_board(InputString),
            io:format("Numbers: ~p~nSymbols: ~p~n", [Numbers, Symbols]),
            io:format("day 2, part 1: ~B~n", [part_one(Numbers, Symbols)]),
            io:format("day 2, part 2: ~B~n", [part_two(Numbers, Symbols)]),
            ok;
        {error, Reason} ->
            {error, string:concat("failed to open day 3 input", atom_to_list(Reason))}
    end.
