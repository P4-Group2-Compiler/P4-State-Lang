(* '%' is a menhir header for direct Ocaml injection of code *)

%{
    open Ast
%}

%token EOF

// Remember IDENTIFIER type string (in this case)
%token STATE
%token <string> IDENTIFIER

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
 | STATE IDENTIFIER {State $2}
;