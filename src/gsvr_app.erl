-module(gsvr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    lager:start(),
    application:start(crypto),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", gsvr_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8090}], [
		{env, [{dispatch, Dispatch}]}
	]),
    gsvr_sup:start_link().

stop(_State) ->
    ok.
