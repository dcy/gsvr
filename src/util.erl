-module(util).
-export([unixtime/0,
         get_ets/2, put_ets/3,
         mapskeydelete/3, mapskeyreplace/4, mapskeyfind/3
        ]).


unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

get_ets(Ets, Key) ->
    case ets:lookup(Ets, Key) of
        [] -> undefined; 
        [{Key, Value}] -> Value
    end.

put_ets(Ets, Key, Value) ->
    ets:insert(Ets, {Key, Value}).


mapskeydelete(What, Key, [H|T]) ->
    case maps:get(Key, H) == What of
        true -> T;
        false -> [H|mapskeydelete(What, Key, T)]
    end;
mapskeydelete(_, _, []) -> [].

mapskeyreplace(What, Key, L, New) when is_list(L), erlang:is_map(New) ->
    mapskeyreplace3(What, Key, L, New).

mapskeyreplace3(What, Key, [H|T], New) ->
    case maps:get(Key, H) == What of
        true -> [New|T];
        false -> [H|mapskeyreplace3(What, Key, T, New)]
    end;
mapskeyreplace3(_, _, [], _) -> [].

mapskeyfind(_What, _Key, []) ->
    false;
mapskeyfind(What, Key, [H|T]) ->
    case maps:get(Key, H) == What of
        true -> H;
        false -> mapskeyfind(What, Key, T)
    end.
