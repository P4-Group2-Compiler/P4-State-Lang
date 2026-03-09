(* '%' is a menhir header for direct Ocaml injection of code *)

%{
    open Ast
%}

%token EOF

%token STATE
%token IDENTIFIER

// Grammatical starting point
%start prog

// Types returned by the AST
%type <Ast.program> prog

%%

prog:
state expr EOF
    { { state = $1; identifier = $2 } }
;

state:
 | STATE expr       {State $2}
;

expr:
 | IDENTIFIER       {Identifier}
;