%% marek

-define(CWBNAME, http_server_cowboy).
-define(CWBHOST, "localhost").
-define(CWBPORT, 8567).

%% logging %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(L_DEBUG(T),     lager:debug(T)).
-define(L_DEBUG(F,D),   lager:debug(F,D)).
-define(L_INFO(T),  	lager:info(T)).
-define(L_INFO(F,D),	lager:info(F,D)).
-define(L_NOTE(T),  	lager:notice(T)).
-define(L_NOTE(F,D),	lager:notice(F,D)).
-define(L_WARN(T),	    lager:warning(T)).
-define(L_WARN(F,D),	lager:warning(F,D)).
-define(L_ERROR(T),	    lager:error(T)).
-define(L_ERROR(F,D),	lager:error(F,D)).
-define(L_CRIT(T),      lager:critical(T)).
-define(L_CRIT(F,D),    lager:critical(F,D)).
-define(L_ALERT(T),	    lager:alert(T)).
-define(L_ALERT(F,D),	lager:alert(F,D)).
-define(L_EMERG(T),	    lager:emergency(T)).
-define(L_EMERG(F,D),	lager:emergency(F,D)).

%% end of file
