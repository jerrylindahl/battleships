-module(battleships).
-author('Jerry Lindahl <jerry@copypasteit.se>').

-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-include("constants.hrl").

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
			
			output:sendGridToPlayer(Socket, Grid, EnemyGrid),
			
			gen_tcp:send(Socket, "Send placement of your ship.\n"),
			
			loop(Socket, Listener, NewEnemy, Grid, EnemyGrid);
			
		{message, Message} ->
			case maps:size(Grid) of
				0-> loop(Socket, Listener, Enemy, putShip(Grid, Message, Socket), EnemyGrid);
				_-> handleTorpedoMessage(Enemy, Message, Socket)
			end,
			loop(Socket, Listener, Enemy, Grid, EnemyGrid);
		
		%TODO: rate limit torpedos to take turns
		%opponent sent a torpedo
		{torpedo, {X, Y}} ->
			gen_tcp:send(Socket, io_lib:format("Incoming torpedo: ~c~p\n", [input:getCharFromX(X), Y])),
			
			{NewGrid, TorpedoResult} = incomingTorpedo(X, Y, Grid),
			Enemy ! {torpedoResult, TorpedoResult, {X,Y}},
			output:sendGridToPlayer(Socket, NewGrid, EnemyGrid),
			
			case countSurvivors(NewGrid) of
				0-> gen_tcp:send(Socket, "LOST THE GAME!\n"),
						Enemy ! {you_win};
				_-> loop(Socket, Listener, Enemy, NewGrid, EnemyGrid)
			end;
		
		%Opponent tells us if we hit or missed
		{torpedoResult, Result, {X, Y}} ->
			gen_tcp:send(Socket, io_lib:format("Torpedo ~c~p ~p!\n", [input:getCharFromX(X), Y, Result])),
			NewEnemyGrid = EnemyGrid#{{X,Y} => Result},
			output:sendGridToPlayer(Socket, Grid, NewEnemyGrid),
			loop(Socket, Listener, Enemy, Grid, NewEnemyGrid);
			
			
		{you_win} ->
			gen_tcp:send(Socket, "YOU WON THE GAME!\n");
		
		{disconnect} ->
			gen_tcp:send(Socket, "Your cowardly opponent has disconnected. You win!\n");
		
		{closed} ->
			Enemy ! {disconnect},
			ok
	end.

handleTorpedoMessage(Enemy, Message, Socket)->
	{ValidPlacement, X, Y} = input:getXYFromBin(Message),
	case ValidPlacement of
		correct-> Enemy ! {torpedo, {X,Y}};
		outside-> gen_tcp:send(Socket, "Invalid coordinates.\n");
		_      -> gen_tcp:send(Socket, "Error while processing message.")
	end.

%update own grid with opponents torpedos
incomingTorpedo(X, Y, Grid)->
	Spot = maps:get({X,Y}, Grid, empty),
	
	case Spot of
		ship  ->{Grid#{{X,Y} => hit}, hit};
		empty ->{Grid#{{X,Y} => miss}, miss};
		miss  ->{Grid, alreadyShotHere};
		_     ->{Grid, invalid}
	end.
			

countSurvivors(Grid)->
	Pred = fun(_,V) -> V =:= ship end,
	maps:size(maps:filter(Pred, Grid)).

%put a ship into a grid, assume to the right of location
%only supports up to 9 in size
%TODO: More than one ship
%TODO: Change direction of ship placement
putShip(Grid, Message, Socket)->
	{ValidPlacement, X, Y} = input:getXYFromBin(Message),
	
	case ValidPlacement of
		correct-> Grid#{{X, Y} => ship, {X+1,Y} => ship};
		_      ->
			gen_tcp:send(Socket, "Incorrect placement, try again.\n"),
			Grid
	end.

listenToPlayer(Parent, Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			Parent ! {message, Data},
			listenToPlayer(Parent, Socket);
			
		{error, closed} ->
			Parent ! {closed}
	end. 
