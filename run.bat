dune build
_build\install\default\bin\P4.exe bin\test.sm
cd output\c
gcc generated_state_machine.c -o StateMachine
StateMachine