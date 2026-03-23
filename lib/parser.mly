(* '%' is a menhir header for direct Ocaml injection of code *)

%{
    open Ast
%}

%token EOF

// Remember IDENTIFIER type string (in this case)
%token STATE
%token <string> IDENTIFIER
%token BINSTATE

// Grammatical starting point
%start prog

// Types returned by the AST
%type <Ast.program> prog

%%

prog:
state EOF
    { $1; }
;

state:
  | STATE BINSTATE IDENTIFIER { State $2 $3 }
;