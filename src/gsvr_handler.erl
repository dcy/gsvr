-module(gsvr_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("gsvr.hrl").


init(_Transport, Req, []) ->
	{ok, Req, undefined}.


handle(Req, State) ->
    {Method, Req} = cowboy_req:method(Req),
    ?TRACE_VAR(Method),
    {ok, Req1} = handle_req(Method, Req),
	{ok, Req1, State}.

handle_req(<<"GET">>, Req) ->
	{AccountId, Req1} = cowboy_req:qs_val(<<"account_id">>, Req),
    do_handle_get(AccountId, Req1);
handle_req(<<"POST">>, Req) ->
    HasBody = cowboy_req:has_body(Req),
    do_handle_post(HasBody, Req);
handle_req(_, Req) ->
    cowboy_req:reply(405, Req).

do_handle_get(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing account_id parameter.">>, Req);
do_handle_get(AccountId, Req) ->
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], AccountId, Req).

do_handle_post(true, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
do_handle_post(false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req).

terminate(_Reason, _Req, _State) ->
	ok.
