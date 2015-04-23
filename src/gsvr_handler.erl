-module(gsvr_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("gsvr.hrl").


init(_Transport, Req, []) ->
	{ok, Req, undefined}.


handle(Req, State) ->
    {Method, Req} = cowboy_req:method(Req),
    {ok, Req1} = handle_req(Method, Req),
	{ok, Req1, State}.

handle_req(<<"GET">>, Req) ->
	{AccountId, Req1} = cowboy_req:qs_val(<<"account_id">>, Req),
    ?TRACE_VAR(AccountId),
    do_handle_get(AccountId, Req1);
handle_req(<<"POST">>, Req) ->
    HasBody = cowboy_req:has_body(Req),
    do_handle_post(HasBody, Req);
handle_req(_, Req) ->
    cowboy_req:reply(405, Req).

do_handle_get(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing account_id parameter.">>, Req);
do_handle_get(AccountId, Req) ->
    Servers = get_account_server_infos(AccountId),
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"application/json; charset=utf-8">>}
	], jsx:encode(Servers), Req).

do_handle_post(true, Req) ->
    {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
    PostVals = jsx:decode(JsonBin, [return_maps]),
    ?TRACE_VAR(PostVals),
    {ok, GsvrToken} = application:get_env(gsvr, token),
    case maps:get(<<"token">>, PostVals, undefined) of
        undefined ->
            cowboy_req:reply(400, [], <<"Missing token parameter.">>, Req2);
        GsvrToken ->
            update_account_servers(Req2, PostVals);
        Other ->
            case is_binary(Other) andalso binary_to_list(Other) =:= GsvrToken of
                true -> update_account_servers(Req2, PostVals);
                false -> cowboy_req:reply(400, [], <<"Token wrong.">>, Req2)
            end
    end;
do_handle_post(false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req).

terminate(_Reason, _Req, _State) ->
	ok.





get_account_server_infos(AccountId) ->
    case util:get_ets(account_servers, AccountId) of
        undefined -> [];
        Infos -> Infos
    end.

update_account_servers(Req, PostVals) ->
    #{<<"account_id">> := AccountId, <<"svr_id">> := SvrId} = PostVals,
    NewInfo = maps:put(<<"time">>, util:unixtime(), maps:remove(<<"token">>, maps:remove(<<"account_id">>, PostVals))),
    Infos = get_account_server_infos(AccountId),
    NewInfos = case util:mapskeyfind(SvrId, <<"svr_id">>, Infos) of
                   false -> [NewInfo | Infos];
                   _Info -> util:mapskeyreplace(SvrId, <<"svr_id">>, Infos, NewInfo)
               end,
    util:put_ets(account_servers, AccountId, NewInfos),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], <<"ok">>, Req).

