#!/bin/sh
rm *.beam
erlc battleships.erl output.erl input.erl input_tests.erl
erl -noshell -s input_tests test -s init stop

# now run with 
# erl
# battleships:listen().
