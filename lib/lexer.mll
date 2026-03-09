{
    open Lexing
    open Parser

    exception Lexing_error of string
}
(* Define chars for the identifier (name of state) *)
let Letter = ['a'-'z' 'A'-'Z']
let IdentifierChars = ['a'-'z' 'A'-'Z' '0'-'9' '_']

(* All the tokenization rules. When lexer hits any of the below, what token should be created *)
rule token = parse
    | "State" { STATE }
    | (Letter IdentifierChars*) as id { IDENTIFIER id }
    | [' ' '\t' '\n'] { token lexbuf } (* Whitespace, tab and newline defined *)
    | eof { EOF } (* end of file; eof from Lexing *)
    | _ as c {raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c))} (* Error when hitting anything else not covered *)