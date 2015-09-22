-module(util).
-export([unixtime/0,
         get_ets/2, put_ets/3,
         to_list/1,
         urlencode/1,
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

to_list( Bin ) when is_binary( Bin ) ->
	binary_to_list( Bin ) ;
to_list( Integer) when is_integer( Integer ) ->
	integer_to_list( Integer ) ;
to_list( Atom ) when is_atom( Atom ) ->
	atom_to_list( Atom )  ;
to_list( Tuple ) when is_tuple( Tuple ) ->
	tuple_to_list( Tuple ) ;
to_list( List ) when is_list( List ) ->
	List ;
to_list( Integer ) when is_integer( Integer) ->
	integer_to_list( Integer ) ;
to_list( Float ) when is_float( Float) ->
	[ String ] = io_lib:format( "~p" , [ Float ] ) ,
	String .

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(Binary) when is_binary(Binary) ->
    quote_plus(binary_to_list(Binary));
quote_plus(Float) when is_float(Float) ->
    quote_plus(mochinum:digits(Float));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

%% @spec urlencode([{Key, Value}]) -> string()
%% @doc URL encode the property list.
urlencode(Props) when is_list(Props) ->
    Pairs = lists:foldr(
              fun ({K, V}, Acc) ->
                      [quote_plus(K) ++ "=" ++ quote_plus(V) | Acc]
              end, [], Props),
    string:join(Pairs, "&");
urlencode(Maps) ->
    Props = maps:to_list(Maps),
    urlencode(Props).
