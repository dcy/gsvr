-module(gsvr_test).
-export([start/0]).
-define(AMOUNT, 13000000).

start() ->
    start(13000000).

start(Acc) when Acc > ?AMOUNT ->
    ok;
start(Acc) ->
    spawn(fun() -> handle_data(Acc) end),
    start(Acc+1).

handle_data(Acc) ->
    AccBin = erlang:integer_to_binary(Acc),
    Data = #{account_id=>AccBin, svr_id=>random:uniform(20), nick=>erlang:integer_to_list(Acc), level=>random:uniform(100), sex=>random:uniform(2), token=>"gsvr_token"},
    Infos = [Data],
    util:put_ets(account_servers, AccBin, Infos).
    
