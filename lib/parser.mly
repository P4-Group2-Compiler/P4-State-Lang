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
| STATE IDENTIFIER LEFTTUBORG transitions RIGHTTUBORG        {{ name = State $2; transitions = $4 }}
;

transitions:
  {[]}
| transition transitions    { $1 :: $2 }
;

transition:
| ON IDENTIFIER GO IDENTIFIER   { Transition (Event $2, State $4) }
;
