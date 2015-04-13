-module(gsvr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include("gsvr.hrl").


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    handle_ets(),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", gsvr_handler, []}
		]}
	]),
    {ok, Port} = application:get_env(gsvr, port),
	{ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
		{env, [{dispatch, Dispatch}]}
	]),
    gsvr_sup:start_link().

stop(_State) ->
    ?TRACE_VAR(stop),
    gsvr:save_data(),
    erlang:halt(),
    ok.

handle_ets() ->
    case filelib:is_file(?ETS_FILE_NAME) of
        true -> ets:file2tab(?ETS_FILE_NAME);
        false -> ets:new(account_servers, [named_table, set, public])
    end.

