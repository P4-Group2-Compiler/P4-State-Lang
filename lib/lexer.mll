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
    (* Whitespace, tab and newline defined *)
    | [' ' '\t' '\n']                       { token lexbuf }

    (* Keywords *)
    | "Statemachine"                        { STATEMACHINE }
    | "State"                               { STATE }
    | "ON"                                  { ON }
    | "GO"                                  { GO }

    (* Seperators *)
    | '{'                                   { LEFTTUBORG }
    | '}'                                   { RIGHTTUBORG }

    (* Identifiers *)
    | (Letter IdentifierChars*) as id       { IDENTIFIER id }

    (* End of file; eof from Lexing *)
    |eof                                    { EOF }

    (* Unexpected Character - Error when hitting anything else not covered *)
    | _ as c {
        raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c))
        }