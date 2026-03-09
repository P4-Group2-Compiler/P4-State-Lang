{
    open Lexing
    open Parser

    exception Lexing_error of string
}

let IdentifierChars = ['a' - 'z' 'A' - 'Z' '0' - '9' '_']


rule token = parse
    | "State"   {STATE}
    | IdentifierChars {IDENTIFIER}

