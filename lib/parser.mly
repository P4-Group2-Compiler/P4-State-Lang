(* '%' is a menhir header for direct Ocaml injection of code *)

%{
    open Ast
%}

%token EOF

// Remember IDENTIFIER type string (in this case)
%token STATE
%token <string> IDENTIFIER
%token TRANSITIONS
%token ON
%token GO

// Punctuators
%token LEFTTUBORG RIGHTTUBORG (* '{' and '}' *)

// Grammatical starting point
%start prog

// Types returned by the AST
%type <Ast.program> prog

%%

prog:
state EOF
    { $1; }
;




transitions:
  {[]}
| transitions transition { $1 @ [$2] }
;

transition:
| ON IDENTIFIER GO IDENTIFIER { Transition ($2, $4) }
;

state:
  | STATE IDENTIFIER LEFTTUBORG TRANSITIONS RIGHTTUBORG {{ name : $2; transitions : $4 }}
;