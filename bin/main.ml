open Lexing
open Parsing
open P4

let () =
  let input = "State q1" in
  let lexbuf = Lexing.from_string input in
  let result = P4.Parser.prog Lexer.token lexbuf in
  match result with
| Ast.State s -> print_endline s
| Ast.StartState s -> print_endline s
| Ast.FinalState s -> print_endline s