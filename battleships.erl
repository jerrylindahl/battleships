-module(battleships).
-author('Jerry Lindahl <jerry@copypasteit.se>').

-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-define(GRID_SIZE, 5). %max 9 for now

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
	loop(Socket, Listener, 0, #{}).
	
% listen to messages from players listen loop and respond
loop(Socket, Listener, Enemy, Grid) ->
	receive
		{init, NewEnemy} ->
			gen_tcp:send(Socket, "Enemy connected!\n"),
			gen_tcp:send(Socket, io_lib:format("Grid size is: ~p\n", [?GRID_SIZE])),
			
			sendGridToPlayer(Socket, Grid),
			
			gen_tcp:send(Socket, "Send placement of your ship.\n"),
			
			loop(Socket, Listener, NewEnemy, Grid);
			
		{message, Message} ->
			case maps:size(Grid) of
				0-> loop(Socket, Listener, Enemy, putShip(Grid, Message));
				_-> Enemy ! {torpedo, getXYFromBin(Message)}
			end,
			loop(Socket, Listener, Enemy, Grid);
		
		%TODO: rate limit torpedos to take turns
		%TODO: keep track of opponent grid
		{torpedo, {X, Y}} ->
			gen_tcp:send(Socket, io_lib:format("Incoming torpedo hit: ~p,~p\n", [X,Y])),
			NewGrid = Grid#{{X,Y} => hit},
			sendGridToPlayer(Socket, NewGrid),
			
			case countSurvivors(NewGrid) of
				0-> gen_tcp:send(Socket, "LOST THE GAME!"),
						Enemy ! {you_win};
				_-> loop(Socket, Listener, Enemy, NewGrid)
			end;
			
		{you_win} ->
			gen_tcp:send(Socket, "YOU WON THE GAME!");
		
		{disconnect} ->
			gen_tcp:send(Socket, "Your cowardly opponent has disconnected. You win!");
		
		{closed} ->
			Enemy ! {disconnect},
			ok
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
getXYFromBin(Bin)->
	X = binary:at(Bin, 0) - 65,
	Y = binary:at(Bin, 1) -  48,
	{X,Y}.

listenToPlayer(Parent, Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			Parent ! {message, Data},
			listenToPlayer(Parent, Socket);
			
		{error, closed} ->
			Parent ! {closed}
	end. 

%kick off sending
sendGridToPlayer(Socket, Grid) ->
	gen_tcp:send(Socket, "  ABCDE\n"),
	sendGridToPlayer(Socket, Grid, 0, 0).

%send grid to player, recursively walks through the grid row wise
sendGridToPlayer(Socket, Grid, X, Y) when X =< ?GRID_SIZE, Y < ?GRID_SIZE->
	Spot = maps:get({X,Y}, Grid, empty),
	
	if X =:= 0 ->
		gen_tcp:send(Socket, io_lib:format("~p ", [Y]));
		true -> ok
	end,
	
	if
			Spot =:= empty  -> gen_tcp:send(Socket, "-");
			Spot =:= hit		-> gen_tcp:send(Socket, "O");
			Spot =:= ship		-> gen_tcp:send(Socket, "X");
			true						-> ok %shouldn't happen, throw error?
	end,
	
	%ugly? refactor?
	Add = if X =:= (?GRID_SIZE-1) ->
		gen_tcp:send(Socket, "\n"),
		1;
		true -> 0
	end,
	
	sendGridToPlayer(Socket, Grid, (X+1) rem ?GRID_SIZE, Y + Add);

sendGridToPlayer(_,_,_,_)->
	ok.
