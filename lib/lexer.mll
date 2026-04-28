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
    | [' ' '\t' '\n' '\r']              { token lexbuf }

    (* Single-line comments *)
    | "//" [^ '\n' '\r']*               { token lexbuf }

    (* Block comments *)
    | "(*"                              { block_comment lexbuf }

    (* Keywords *)
| "Statemachine"                        { STATEMACHINE }
| "Start"                               { START }
| "Final"                               { FINAL }
| "State"                               { STATE }
| "ON"                                  { ON }
| "GO"                                  { GO }
| "IF"                                  { IF }
| "#"                                   { HASH }
| "VAR"                                 { VAR }

(* Identifiers *)
| (Letter IdentifierChars*) as id       { IDENTIFIER id }

(* Numbers *)
| ['0'-'9']+ as n                       { INT n }


(* Binops *)
| '+'                                   { PLUS }
| '-'                                   { MINUS }
| '*'                                   { TIMES }
| "/"                                   { DIV }
| '%'                                   { MOD } 
| '='                                   { EQUAL }
(*| "=="                                { BEQUAL }
| "!="                                  { BNEQUAL }*)
| "<"                                   { LT }
| "<="                                  { LTE }
| ">"                                   { GT }
| ">="                                  { GTE }    

(* Seperators *)
| '{'                                   { LEFTTUBORG }
| '}'                                   { RIGHTTUBORG }
| '('                                   { LP }  
| ')'                                   { RP }

|eof                                    { EOF }

(* Unexpected Character *)
| _ as c {
    raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c))
}

(* Rule for block comments *)
and block_comment = parse
    | "*)"                              { token lexbuf }
    | eof {
        raise (Lexing_error "Unterminated block comment")
      }
    | _                                 { block_comment lexbuf }