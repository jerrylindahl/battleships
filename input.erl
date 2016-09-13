-module(input).
-author('Jerry Lindahl <jerry@copypasteit.se>').

-export([boundCheckCoordinates/2, getXYFromBin/1, getCharFromX/1]).

-include("constants.hrl").

-define(ASCII_0, 48).
-define(ASCII_A, 65).

-define(ASCII_a, 97).
-define(ASCII_z, 122).

-define(ASCII_a_to_A, 32). %offset between A and a


%get a set of coordinates from user message
%TODO: error handling
%TODO: handle both "a1" and "A1"
getXYFromBin(Bin)->
	X = binary:at(Bin, 0), % - 65,
	Y = binary:at(Bin, 1), % -  48,
	convertCoordinatesFromASCII(boundCheckCoordinates(X,Y)).


convertCoordinatesFromASCII({ValidPlacement, X, Y})->
	case ValidPlacement of
		correct->{ValidPlacement, X-?ASCII_A, Y-?ASCII_0};
		_->{ValidPlacement, X, Y}
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
	{outside, X-?ASCII_A, Y-?ASCII_0}.


%Convert an X coordinate to letter for printing
getCharFromX(X)->
	X+?ASCII_A.
