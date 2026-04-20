(* '%' is a menhir header for direct Ocaml injection of code *)

%{
    open Ast
%}

// Token Decleration

%token EOF

(* Identifiers and integers *)
%token <string> IDENTIFIER
%token <string> INT

(* Keywords *)
%token STATEMACHINE
%token STATE
%token START
%token FINAL
%token ON
%token GO
%token IF

(* Operators *)
%token LT

// Punctuators
%token LEFTTUBORG RIGHTTUBORG (* '{' and '}' *)

// Grammatical starting point
%start prog

// Types returned by the AST
%type <Ast.program> prog
%type <Ast.expr> expr

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
| ON IDENTIFIER IF expr GO IDENTIFIER
      { 
        Transition (Event $2, Expr $4 State $6)
      }
;

expr:
  | IDENTIFIER
      { 
        (* for now, ignore locations and just fake them *)
        let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos) in
        Eident { loc = dummy_loc; id = $1 }
      }
  | INT
      {
        Ecst (Cint (int_of_string $1))
      }
  | expr LT expr
      {
        Ebinop (Blt, $1, $3)
      }
;

ident:
  id = IDENT { { loc = ($startpos, $endpos); id } }
;