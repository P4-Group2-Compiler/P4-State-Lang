@echo off
setlocal

del /q /f output\c\generated_state_machine.c 2>nul
del /q /f output\c\StateMachine.exe 2>nul
del /q /f output\c\StateMachine 2>nul
del /q /f output\DOT\graph.gv 2>nul
del /q /f output\DOT\graph.png 2>nul

dune clean || exit /b %errorlevel%
dune build || exit /b %errorlevel%

dune exec bin/main.exe -- bin/test.sm || exit /b %errorlevel%

cd /d output\c || exit /b %errorlevel%

gcc generated_state_machine.c -o StateMachine.exe || exit /b %errorlevel%
StateMachine.exe || exit /b %errorlevel%

endlocal