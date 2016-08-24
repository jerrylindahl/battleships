Trying to learn some Erlang by implementing a network battleship game.

Compile and run with
  jerry@Loke:~$ erl
  1>c(battleships).
  {ok,battleships}
  2>battleships:listen(1050).
  

From other console:
>telnet localhost 1050

From yet another console:
>telnet localhost 1050

Requires Erlang 17 due to usage of maps.

Not feature complete, missing features documented with "TODO:"
