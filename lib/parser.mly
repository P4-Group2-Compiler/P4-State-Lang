(* '%' is a menhir header for direct Ocaml injection of code *)

%{
    open Ast
%}

// Token Decleration

%token EOF

// Remember IDENTIFIER type string (in this case)
%token <string> IDENTIFIER

// Keywords
%token STATEMACHINE
%token STATE
%token <string> START 
%token <string> FINAL
%token ON
%token GO

// Punctuators
%token LEFTTUBORG RIGHTTUBORG (* '{' and '}' *)

// Grammatical starting point
%start prog

// Types returned by the AST
%type <Ast.program> prog

%%

// Grammar Rules

prog:
| STATEMACHINE IDENTIFIER LEFTTUBORG states RIGHTTUBORG EOF  { {machine_name = $2; states = $4} }
;

states:
| { [] }
| state states  { $1 :: $2 }
;

state:
| state_kind STATE IDENTIFIER LEFTTUBORG transitions RIGHTTUBORG  {{ kind = $1; name = State $3; transitions = $5 }}

;

state_kind:
| START { Start }
| FINAL { Final }
|       { Normal } // Empty means that there is no State Kind

transitions:
  {[]}
| transition transitions    { $1 :: $2 }
;

transition:
| ON IDENTIFIER GO IDENTIFIER   { Transition (Event $2, State $4) }
;
