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
	{Aid, Req1} = cowboy_req:qs_val(<<"aid">>, Req),
    case Aid of
        undefined -> handle_sauth_and_get(Req1);
        _ -> do_handle_get(Aid, Req1)
    end;
handle_req(<<"POST">>, Req) ->
    HasBody = cowboy_req:has_body(Req),
    do_handle_post(HasBody, Req);
handle_req(_, Req) ->
    cowboy_req:reply(405, Req).

do_handle_get(undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing aid parameter.">>, Req);
do_handle_get("", Req) ->
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"application/json; charset=utf-8">>}
	], jsx:encode([]), Req);
do_handle_get(Aid, Req) ->
    Servers = get_account_server_infos(Aid),
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"application/json; charset=utf-8">>}
	], jsx:encode(Servers), Req).


do_handle_post(true, Req) ->
    {ok, [{JsonBin, true}], Req2} = cowboy_req:body_qs(Req),
    PostVals = jsx:decode(JsonBin, [return_maps]),
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





get_account_server_infos(Aid) ->
    case util:get_ets(account_servers, util:to_list(Aid)) of
        undefined -> [];
        Infos -> Infos
    end.

update_account_servers(Req, PostVals) ->
    #{<<"aid">> := Aid, <<"svr_id">> := SvrId} = PostVals,
    NewInfo = maps:put(<<"time">>, util:unixtime(), maps:remove(<<"token">>, maps:remove(<<"aid">>, PostVals))),
    Infos = get_account_server_infos(Aid),
    NewInfos = case util:mapskeyfind(SvrId, <<"svr_id">>, Infos) of
                   false -> [NewInfo | Infos];
                   _Info -> util:mapskeyreplace(SvrId, <<"svr_id">>, Infos, NewInfo)
               end,
    util:put_ets(account_servers, Aid, NewInfos),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], <<"ok">>, Req).

handle_sauth_and_get(Req) ->
    %?TRACE_VAR(cowboy_req:qs_vals(Req)),
    %?TRACE_VAR(cowboy_req:qs(Req)),
    {QsBin, _} = cowboy_req:qs(Req),
    {{IpTuple, _}, _} = cowboy_req:peer(Req),
    IpList = [integer_to_list(Item) || Item <- tuple_to_list(IpTuple)],
    Ip = string:join(IpList, "."),
    ArgsStr = util:urlencode([{gameid, "ma37"}, {hostid, 0}, {ip, Ip}]),
    QsStr = binary_to_list(QsBin) ++ "&" ++ ArgsStr,

    SauthHost = "http://123.58.175.237:7040/sauth",
    URL = erlang:list_to_binary(lists:concat([SauthHost, "?", QsStr])),
    Method = get,
    Headers = [],
    Payload = <<>>,
    Options = [{pool, default}],

    Aid = case hackney:request(Method, URL, Headers, Payload, Options) of
              {ok, _StatusCode, _RespHeaders, ClientRef} ->
                  {ok, Body} = hackney:body(ClientRef),
                  Result = jsx:decode(Body, [return_maps]),
                  case maps:get(<<"code">>, Result) of
                      200 ->
                          #{<<"aid">>:=AidOri} = Result,
                          util:to_list(AidOri);
                      _ ->
                          ?ERROR_MSG("sauth_fail, QsStr:~p, reason: ~p",
                                     [QsStr, Result]),
                          ""
                  end;
              Result ->
                  ?ERROR_MSG("sauth_fail, QsStr:~p, reason: ~p",
                             [QsStr, Result]),
                  ""
          end,
    do_handle_get(Aid, Req).
