open Lexing
open Parsing

let () =
  let input = "State q1" in
  let tokens = Lexer.tokenize input in
  let ast = Parser.parse tokens in
  Ast.pretty_print ast
