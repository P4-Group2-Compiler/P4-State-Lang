#!/usr/bin/env sh

set -e

rm -f output/c/generated_state_machine.c
rm -f output/c/StateMachine
rm -f output/DOT/graph.gv
rm -f output/DOT/graph.png

dune clean
dune build

dune exec bin/main.exe -- bin/demoSimple.sm

cd output/c
gcc generated_state_machine.c -o StateMachine
./StateMachine