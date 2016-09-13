-module(output).
-author('Jerry Lindahl <jerry@copypasteit.se>').

-export([sendGridToPlayer/3, sendGridLetterIndex/1]).

-include("constants.hrl").


%kick off sending
sendGridToPlayer(Socket, Grid, Grid2) ->
  ?PRINT(Grid2),
	sendGridLetterIndex(Socket),
	sendGridToPlayer(Socket, Grid, Grid2, 0, 0).

%send grid to player, recursively walks through the grid row wise
%prints them side by side so will walk X up to 2*GRID_SIZE
sendGridToPlayer(Socket, Grid, Grid2, X, Y) when X =< ?GRID_SIZE*2, Y < ?GRID_SIZE->
	%send which row we are on for edge of game board
	if X =:= 0 ->
		gen_tcp:send(Socket, io_lib:format("~p ", [Y]));
		true -> ok
	end,

	%TODO: break out into sendRow function.
	Spot = if X < ?GRID_SIZE ->
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
