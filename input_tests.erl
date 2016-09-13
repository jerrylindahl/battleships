-module(input_tests).
-author('Jerry Lindahl <jerry@copypasteit.se>').

-include_lib("eunit/include/eunit.hrl").

getCharFromX_test() ->
	67 = input:getCharFromX(2).

getXYFromBin_test() ->
  ?assertEqual({correct, 0, 0}, input:getXYFromBin(<<"A0\r\n">>)),
  ?assertEqual({correct, 5, 5}, input:getXYFromBin(<<"F5\r\n">>)),
  ?assertEqual({correct, 3, 3}, input:getXYFromBin(<<"D3\r\n">>)),
  ?assertEqual({outside, 90, 50}, input:getXYFromBin(<<"Z2\r\n">>)),
  
  %the function takes the first digit and ignores rest of input so the
  %will generate correct
  ?assertEqual({correct, 0, 1}, input:getXYFromBin(<<"A11\r\n">>)),
  ?assertEqual({outside, 90, 49}, input:getXYFromBin(<<"Z12\r\n">>)).
