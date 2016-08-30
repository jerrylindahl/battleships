-module(battleships).
-author('Jerry Lindahl <jerry@copypasteit.se>').

-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-define(GRID_SIZE, 6). %max 9 for now

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

% Call echo:listen(Port) to start the service.
listen(Port) ->
	{ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	accept(LSocket).

% Wait for 2 incoming connections
accept(LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	accept(LSocket, spawn(fun() -> loop(Socket, 0, 0) end)).
accept(LSocket, Player1) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	accept(LSocket, Player1, spawn(fun() -> loop(Socket, 0, 0) end)).
accept(LSocket, Player1, Player2) ->
	Player1 ! {init, Player2},
	Player2 ! {init, Player1},
	ok = gen_tcp:close(LSocket).

% kick off main loop
loop(Socket, 0, 0) ->
	Self = self(),
	Listener = spawn(fun() -> listenToPlayer(Self, Socket) end),
	loop(Socket, Listener, 0, #{}, #{}).
	
% listen to messages from players listen loop and respond
loop(Socket, Listener, Enemy, Grid, EnemyGrid) ->
	receive
		{init, NewEnemy} ->
			gen_tcp:send(Socket, "Enemy connected!\n"),
			gen_tcp:send(Socket, io_lib:format("Grid size is: ~p\n", [?GRID_SIZE])),
			
			sendGridToPlayer(Socket, Grid, EnemyGrid),
			
			gen_tcp:send(Socket, "Send placement of your ship.\n"),
			
			loop(Socket, Listener, NewEnemy, Grid, EnemyGrid);
			
		{message, Message} ->
			case maps:size(Grid) of
				0-> loop(Socket, Listener, Enemy, putShip(Grid, Message), EnemyGrid);
				_-> Enemy ! {torpedo, getXYFromBin(Message)}
			end,
			loop(Socket, Listener, Enemy, Grid, EnemyGrid);
		
		%TODO: rate limit torpedos to take turns
		%opponent sent a torpedo
		{torpedo, {X, Y}} ->
			gen_tcp:send(Socket, io_lib:format("Incoming torpedo: ~c~p\n", [getCharFromX(X), Y])),
			
			{NewGrid, TorpedoResult} = incomingTorpedo(X, Y, Grid),
			Enemy ! {torpedoResult, TorpedoResult, {X,Y}},
			sendGridToPlayer(Socket, NewGrid, EnemyGrid),
			
			case countSurvivors(NewGrid) of
				0-> gen_tcp:send(Socket, "LOST THE GAME!"),
						Enemy ! {you_win};
				_-> loop(Socket, Listener, Enemy, NewGrid, EnemyGrid)
			end;
		
		%Opponent tells us if we hit or missed
		{torpedoResult, Result, {X, Y}} ->
			gen_tcp:send(Socket, io_lib:format("Torpedo ~c~p ~p!\n", [getCharFromX(X), Y, Result])),
			NewEnemyGrid = EnemyGrid#{{X,Y} => Result},
			sendGridToPlayer(Socket, Grid, NewEnemyGrid),
			loop(Socket, Listener, Enemy, Grid, NewEnemyGrid);
			
			
		{you_win} ->
			gen_tcp:send(Socket, "YOU WON THE GAME!");
		
		{disconnect} ->
			gen_tcp:send(Socket, "Your cowardly opponent has disconnected. You win!");
		
		{closed} ->
			Enemy ! {disconnect},
			ok
	end.

%update own grid with opponents torpedos
incomingTorpedo(X, Y, Grid)->
	Spot = maps:get({X,Y}, Grid, empty),
	
	case Spot of
		ship->{Grid#{{X,Y} => hit}, hit};
		empty->{Grid#{{X,Y} => miss}, miss}
	end.
			

countSurvivors(Grid)->
	Pred = fun(_,V) -> V =:= ship end,
	maps:size(maps:filter(Pred, Grid)).

%put a ship into a grid, assume to the right of location
%only supports up to 9 in size
%TODO: bound checking
%TODO: More than one ship
%TODO: Change direction of ship placement
putShip(Grid, Message)->
	{X, Y} = getXYFromBin(Message),
	Grid#{{X, Y} => ship, {X+1,Y} => ship}.

%get a set of coordinates from user message
%TODO: error handling
%TODO: handle both "a1" and "A1"
getXYFromBin(Bin)->
	X = binary:at(Bin, 0) - 65,
	Y = binary:at(Bin, 1) -  48,
	{X,Y}.

%Convert an X coordinate to letter for printing
getCharFromX(X)->
	X+65.

listenToPlayer(Parent, Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			Parent ! {message, Data},
			listenToPlayer(Parent, Socket);
			
		{error, closed} ->
			Parent ! {closed}
	end. 

%kick off sending
sendGridToPlayer(Socket, Grid, Grid2) ->
	sendGridLetterIndex(Socket),
	sendGridToPlayer(Socket, Grid, Grid2, 0, 0).

	%send grid to player, recursively walks through the grid row wise
	%prints them side by side so will walk X up to 2*GRID_SIZE
	sendGridToPlayer(Socket, Grid, Grid2, X, Y) when X =< ?GRID_SIZE*2, Y =< ?GRID_SIZE->
	%send which row we are on for edge of game board
	if X =:= 0 ->
		gen_tcp:send(Socket, io_lib:format("~p ", [Y]));
		true -> ok
	end,

	%TODO: break out into sendRow function.
	Spot = if X =< ?GRID_SIZE ->
		maps:get({X,Y}, Grid, empty);
		true -> maps:get({(X rem ?GRID_SIZE),Y}, Grid2, empty)
	end,

	if
			Spot =:= empty  -> gen_tcp:send(Socket, "-");
			Spot =:= hit		-> gen_tcp:send(Socket, "X");
			Spot =:= ship		-> gen_tcp:send(Socket, "#");
			Spot =:= miss		-> gen_tcp:send(Socket, "O");
			true						-> ok %shouldn't happen, throw error?
	end,
	
	%if end of first grid send spaces before printing next grid
	if X =:= ((?GRID_SIZE)-1) ->
		gen_tcp:send(Socket, "   ");
		true -> ok
	end,
	
	%If end of line, send newline and increase Y counter
	%ugly? refactor?
	Add = if X =:= ((?GRID_SIZE)*2-1) ->
		gen_tcp:send(Socket, "\n"),
		1;
		true -> 0
	end,
	
	sendGridToPlayer(Socket, Grid, Grid2, (X+1) rem (?GRID_SIZE*2), Y + Add);

sendGridToPlayer(_,_,_,_,_)->
	ok.

% send the ABCDEF... index above the grid
sendGridLetterIndex(Socket)->
	Letters = lists:seq(65, 64+?GRID_SIZE),
	gen_tcp:send(Socket, io_lib:format("  ~s   ~s\n", [Letters, Letters])).
