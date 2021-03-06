-module(input).
-author('Jerry Lindahl <jerry@copypasteit.se>').

-export([boundCheckCoordinates/2, getXYFromBin/1, getCharFromX/1]).

-include("constants.hrl").


%get a set of coordinates from user message
getXYFromBin(Bin)->
	X = binary:at(Bin, 0),
	Y = binary:at(Bin, 1),
	convertCoordinatesFromASCII(boundCheckCoordinates(X,Y)).


convertCoordinatesFromASCII({ValidPlacement, X, Y})->
	case ValidPlacement of
		correct-> {ValidPlacement, X-?ASCII_A, Y-?ASCII_0};
		_      -> {ValidPlacement, X, Y}
	end.
	
%User typed 'a' instead of 'A'
boundCheckCoordinates(X, Y) when X =< ?ASCII_z, X >= ?ASCII_a ->
	boundCheckCoordinates(X-?ASCII_a_to_A, Y);

%user is within grid
boundCheckCoordinates(X, Y) when 
	X < (?ASCII_A + ?GRID_SIZE),
	X >= ?ASCII_A,
	Y < (?ASCII_0 + ?GRID_SIZE),
	Y >= ?ASCII_0 ->
	{correct, X,Y};

boundCheckCoordinates(X, Y) ->
	{outside, X, Y}.


%Convert an X coordinate to letter for printing
getCharFromX(X)->
	X+?ASCII_A.
