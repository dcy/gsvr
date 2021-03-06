-define(DEBUG_MSG(Str), lager:debug(Str)).
-define(DEBUG_MSG(Format, Args), lager:debug(Format, Args)).
-define(INFO_MSG(Str), lager:info(Str)).
-define(INFO_MSG(Format, Args), lager:info(Format, Args)).
-define(NOTICE_MSG(Str), lager:notice(Str)).
-define(NOTICE_MSG(Format, Args), lager:notice(Format, Args)).
-define(WARNING(Str), lager:warning(Str)).
-define(WARNING_MSG(Format, Args), lager:warning(Format, Args)).
-define(ERROR_MSG(Str), lager:error(Str)).
-define(ERROR_MSG(Format, Args), lager:error(Format, Args)).
-define(CRITICAL_MSG(Str), lager:critical(Str)).
-define(CRITICAL_MSG(Format, Args), lager:critical(Format, Args)).
-define(ALERT_MSG(Str), lager:alert(Str)).
-define(ALERT_MSG(Format, Args), lager:alert(Format, Args)).

-define(TRACE_VAR(Arg), io:format("~n******~nModule: ~p, Line: ~p, ~nMy print's ~p is ~p~n******~n", [?MODULE, ?LINE, ??Arg, Arg])).

-define(ETS_FILE_NAME, "account_servers.ets").
