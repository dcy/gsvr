-module(gsvr).
-export([start/0, stop_node/1, stop/0, save_data/0]).

-include("gsvr.hrl").

start() ->
    start_applications(),
    ok.

stop_node([Node]) ->
    ?TRACE_VAR(net_adm:ping(Node)),
    net_adm:ping(Node),
    rpc:call(Node, gsvr, stop, []),
    erlang:halt(),
    ok.

stop() ->
    application:stop(gsvr),
    ok.

start_applications() ->
    lager:start(),
    %application:start(erlcron),
    %application:start(crypto),
    %application:start(ranch),
    %application:start(cowlib),
    %application:start(cowboy),
    %application:start(gsvr),
    application:ensure_all_started(gsvr),
    ok.

save_data() ->
    ?TRACE_VAR(save_data),
    ets:tab2file(account_servers, ?ETS_FILE_NAME),
    ok.

