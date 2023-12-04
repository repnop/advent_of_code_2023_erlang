-module(day1).

-export([run/0]).

-spec first_last_digit_combined(string()) -> integer().
first_last_digit_combined(String) ->
    Numbers =
        lists:filtermap(fun(Char) ->
                           case string:to_integer([Char]) of
                               {error, _} -> false;
                               {Number, _} -> {true, Number}
                           end
                        end,
                        String),
    [First | Rest] = Numbers,
    {Integer, _} =
        string:to_integer(
            string:concat(integer_to_list(First),
                          integer_to_list(case length(Rest) of
                                              0 ->
                                                  First;
                                              _ ->
                                                  lists:last(Rest)
                                          end))),
    Integer.

-spec part_one(string()) -> integer().
part_one(Input) ->
    InputLines = string:lexemes(Input, "\n"),
    lists:foldr(fun(N, Acc) -> N + Acc end,
                0,
                lists:map(fun first_last_digit_combined/1, InputLines)).

-spec digitize_line([], string()) -> string();
                   (nonempty_list(), string()) -> string().
digitize_line([], Return) ->
    Return;
digitize_line(String, Return) ->
    DigitMap =
        [{"one", "1"},
         {"two", "2"},
         {"three", "3"},
         {"four", "4"},
         {"five", "5"},
         {"six", "6"},
         {"seven", "7"},
         {"eight", "8"},
         {"nine", "9"}],
    case lists:search(fun({Word, _}) -> lists:prefix(Word, String) end, DigitMap) of
        {value, {Word, Digit}} ->
            {_, Rest} = lists:split(length(Word) - 1, String),
            digitize_line(Rest, lists:append(Return, Digit));
        false ->
            [Char | Rest] = String,
            digitize_line(Rest, lists:append(Return, [Char]))
    end.

-spec digitize_line(string()) -> string().
digitize_line(String) ->
    digitize_line(String, []).

-spec part_two(string()) -> integer().
part_two(Input) ->
    InputLines = string:lexemes(Input, "\n"),
    DigitizedLines = lists:map(fun digitize_line/1, InputLines),
    lists:foldr(fun(N, Acc) -> N + Acc end,
                0,
                lists:map(fun first_last_digit_combined/1, DigitizedLines)).

-spec run() -> ok | {error, string()}.
run() ->
    case file:read_file("input/day1.txt") of
        {ok, Input} ->
            InputString = binary_to_list(Input),
            io:format("day 1, part 1: ~B~n", [part_one(InputString)]),
            io:format("day 1, part 2: ~B~n", [part_two(InputString)]),
            ok;
        {error, Reason} ->
            {error, string:concat("failed to open day 1 input", atom_to_list(Reason))}
    end.
