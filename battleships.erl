-module(battleships).
-author('Jerry Lindahl <jerry@copypasteit.se>').

-export([listen/0, listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-include("constants.hrl").

listen() ->
	listen(1050).

listen(Port) ->
	{ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	accept(LSocket).

% Wait for 2 incoming connections
accept(LSocket) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	output:sendWaitMessage(Socket),
	accept(LSocket, spawn(fun() -> loop(Socket, 0, 0) end)).
accept(LSocket, Player1) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	init(LSocket, Player1, spawn(fun() -> loop(Socket, 0, 0) end)).

%Tell the players to have a go. Inform the threads about their enemy
init(LSocket, Player1, Player2) ->
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
			
			gen_tcp:send(Socket, "Send placement of your ships Example: A1.\n"),
			
			loop(Socket, Listener, NewEnemy, Grid, EnemyGrid);
			
		{message, Message} ->
			case allShipsPlaced(Grid) of
				%TODO: need to implement a state machine to keep track of when user is in 
				%placing of ships stage and how many more they need to place, and to allow 
				%retries when placing ships incorrectly (handled already but not in a user
				%friendly way). Also needed for more useful messages like 
				%	"All ships placed, ready to fire!"
				false-> loop(Socket, Listener, Enemy, putShip(Grid, Message, Socket), EnemyGrid);
				true-> handleTorpedoMessage(Enemy, Message, Socket)
			end,
			loop(Socket, Listener, Enemy, Grid, EnemyGrid);
		
		%TODO: rate limit torpedos to take turns (or maybe not, now there are 
		%essentially two game modes, Gentleman where you take turns, and 
		%Gun blazin where you fire as fast as possible)
		%opponent sent a torpedo
		{torpedo, {X, Y}} ->
			case allShipsPlaced(Grid) of
				false->
					%shot at before placed all my ships? How rude!
					Enemy ! {torpedoResult, notReady, {X,Y}},
					loop(Socket, Listener, Enemy, Grid, EnemyGrid);
				true->
					gen_tcp:send(Socket, io_lib:format("Incoming torpedo: ~c~p\n", [input:getCharFromX(X), Y])),
					
					{NewGrid, TorpedoResult} = incomingTorpedo(X, Y, Grid),
					Enemy ! {torpedoResult, TorpedoResult, {X,Y}},
					output:sendGridToPlayer(Socket, NewGrid, EnemyGrid),
					
					case countSurvivors(NewGrid) of
						0-> gen_tcp:send(Socket, "LOST THE GAME!\n"),
								Enemy ! {you_win};
						_-> loop(Socket, Listener, Enemy, NewGrid, EnemyGrid)
					end
			end;
		
		%Opponent tells us if we hit or missed
		{torpedoResult, Result, {X, Y}} ->
			case Result of
				notReady -> 
					gen_tcp:send(Socket, "Opponent hasn't placed their ships yet."),
					loop(Socket, Listener, Enemy, Grid, EnemyGrid);
				alreadyShotHere ->
					gen_tcp:send(Socket, "You already shot there."),
					loop(Socket, Listener, Enemy, Grid, EnemyGrid);
				_ ->
					gen_tcp:send(Socket, io_lib:format("Torpedo ~c~p ~p!\n", [input:getCharFromX(X), Y, Result])),
					NewEnemyGrid = EnemyGrid#{{X,Y} => Result},
					output:sendGridToPlayer(Socket, Grid, NewEnemyGrid),
					loop(Socket, Listener, Enemy, Grid, NewEnemyGrid)
			end;
			
			
		{you_win} ->
			gen_tcp:send(Socket, "YOU WON THE GAME!\n");
		
		{disconnect} ->
			gen_tcp:send(Socket, "Your cowardly opponent has disconnected. You win!\n");
		
		{closed} ->
			Enemy ! {disconnect},
			ok
	end.

%TODO: break out all this grid manipulation stuff into module.

%Handle torpedo message containing coordinates from user
%message is in binary. Example: <<"B1\r\n">>
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
			
%if user has placed all ships on the grid yet.
allShipsPlaced(Grid)->
	Size = maps:size(Grid),
	if 
		Size < 5 -> false;
		true                -> true
	end.

countSurvivors(Grid)->
	Pred = fun(_,V) -> V =:= ship end,
	maps:size(maps:filter(Pred, Grid)).

%put a ship into a grid, assume to the right of location
%only supports up to 9 in size
%TODO: Change direction of ship placement
putShip(Grid, Message, Socket)->
	{ValidPlacement1, X, Y} = input:getXYFromBin(Message),
	%boundcheck work with ascii numbers so we need to convert up to ascii again.
	{ValidPlacement2, _, _} = input:boundCheckCoordinates(X+1+?ASCII_A, Y+?ASCII_0),
	Spot1 = maps:get({X,Y}, Grid, empty),
	Spot2 = maps:get({X+1, Y}, Grid, empty),
	
	if 
		ValidPlacement1 =:= correct,
		ValidPlacement2 =:= correct,
		Spot1 =:= empty,
		Spot2 =:= empty ->
			Grid#{{X, Y} => ship, {X+1,Y} => ship};
		true ->
			gen_tcp:send(Socket, "Incorrect placement, try again.\n"),
			Grid
	end.

%Loop to listen for player messages and pass them on to parent
listenToPlayer(Parent, Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} ->
			Parent ! {message, Data},
			listenToPlayer(Parent, Socket);
			
		{error, closed} ->
			Parent ! {closed}
	end. 
