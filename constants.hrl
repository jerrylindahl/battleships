-define(GRID_SIZE, 6). %max 9 for now

-define(ASCII_0, 48).
-define(ASCII_A, 65).

-define(ASCII_a, 97).
-define(ASCII_z, 122).

-define(ASCII_a_to_A, 32). %offset between A and a


-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.
