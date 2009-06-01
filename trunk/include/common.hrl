% define macros and options common to all erl programs

% provides debugging macros
-ifdef(debug).
-define(DBUG(X), io:format("DEBUG ~p:~p ~p~n", [?MODULE, ?LINE, X])).
-else.
-define(DBUG(X), void).
-endif.

