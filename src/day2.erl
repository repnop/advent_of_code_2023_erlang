-module(day2).

-export([run/0]).

-record(roll, {red :: integer(), green :: integer(), blue :: integer()}).
-record(game, {id :: integer(), rolls :: [#roll{}]}).
-record(dicecounts, {red :: integer(), green :: integer(), blue :: integer()}).

-spec parse_roll(string()) -> #roll{}.
parse_roll(Roll) ->
    Colors = lists:map(fun(R) -> string:strip(R) end, string:lexemes(Roll, ",")),
    RollValues =
        lists:foldr(fun(CountAndColor, Acc) ->
                       [Count | ColorL] = string:lexemes(CountAndColor, " "),
                       [ColorName | _] = ColorL,
                       case string:to_integer(Count) of
                           {error, _} -> error("bad integer");
                           {CountValue, _} ->
                               case ColorName of
                                   "red" -> Acc#roll{red = CountValue};
                                   "green" -> Acc#roll{green = CountValue};
                                   "blue" -> Acc#roll{blue = CountValue};
                                   _ -> error("bad color")
                               end
                       end
                    end,
                    #roll{red = 0,
                          green = 0,
                          blue = 0},
                    Colors),
    RollValues.

-spec parse_game(string()) -> #game{}.
parse_game(Line) ->
    [GameWithId | GamesL] = string:split(Line, ": "),
    [Games | _] = GamesL,
    IdString = string:slice(GameWithId, 5),
    Id = case string:to_integer(IdString) of
             {error, _} ->
                 error("bad game ID");
             {GameId, _} ->
                 GameId
         end,
    GameList = string:lexemes(Games, ";"),
    Rolls = lists:map(fun parse_roll/1, GameList),
    #game{id = Id, rolls = Rolls}.

-spec part_one([#game{}]) -> integer().
part_one(Games) ->
    ValidGames =
        lists:filtermap(fun(#game{id = Id, rolls = Rolls}) ->
                           AllRulesInBounds =
                               lists:all(fun(#roll{blue = Blue,
                                                   green = Green,
                                                   red = Red}) ->
                                            (Red =< 12) and (Green =< 13) and (Blue =< 14)
                                         end,
                                         Rolls),
                           case AllRulesInBounds of
                               true -> {true, Id};
                               false -> false
                           end
                        end,
                        Games),
    lists:foldr(fun(Id, Acc) -> Id + Acc end, 0, ValidGames).

-spec part_two([#game{}]) -> integer().
part_two(Games) ->
    MinDiceCounts =
        lists:map(fun(Game) ->
                     lists:foldr(fun(Roll, Acc) ->
                                    Acc#dicecounts{red = max(Roll#roll.red, Acc#dicecounts.red),
                                                   green =
                                                       max(Roll#roll.green, Acc#dicecounts.green),
                                                   blue = max(Roll#roll.blue, Acc#dicecounts.blue)}
                                 end,
                                 #dicecounts{blue = 0,
                                             green = 0,
                                             red = 0},
                                 Game#game.rolls)
                  end,
                  Games),
    lists:foldr(fun(MinCounts, Acc) ->
                   Acc
                   + MinCounts#dicecounts.red
                     * MinCounts#dicecounts.green
                     * MinCounts#dicecounts.blue
                end,
                0,
                MinDiceCounts).

-spec run() -> ok | {error, string()}.
run() ->
    case file:read_file("input/day2.txt") of
        {ok, Input} ->
            InputString = binary_to_list(Input),
            Games = lists:map(fun parse_game/1, string:lexemes(InputString, "\n")),
            io:format("day 2, part 1: ~B~n", [part_one(Games)]),
            io:format("day 2, part 2: ~B~n", [part_two(Games)]),
            ok;
        {error, Reason} ->
            {error, string:concat("failed to open day 2 input", atom_to_list(Reason))}
    end.
