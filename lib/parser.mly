(* '%' is a menhir header for direct Ocaml injection of code *)

%{
    open Ast
%}

// Token Declaration

%token EOF

// Identifiers and integers
%token <string> IDENTIFIER
%token <string> INT

// Keywords
%token STATEMACHINE
%token STATE
%token START
%token FINAL
%token STARTFINAL
%token ON
%token GO
%token IF
%token HASH
%token VAR
%token AND
%token OR
%token INPUT
%token DO
%token AUTO

// Operators
%token BEQUAL BNEQUAL LTE GT GTE LT  
%token PLUS MINUS TIMES

// Punctuators
%token LEFTTUBORG RIGHTTUBORG (* '{' and '}' *)
%token LP RP (* COMMA *) EQUAL (* "("     ")"     ","     "="  *)

// Precedence rules
%left OR
%left AND
%nonassoc BEQUAL BNEQUAL LT LTE GT GTE
%left PLUS MINUS
%left TIMES


// Grammatical starting point
%start prog

// Types returned by the AST
%type <Ast.program> prog
%type <Ast.expr> expr

// Grammar Rules
%%

prog:
| STATEMACHINE IDENTIFIER LEFTTUBORG variables inputs states RIGHTTUBORG EOF  { {machine_name = $2; variables = $4; inputs = $5; states = $6} }
;

states:
| { [] }
| state states  { $1 :: $2 }
;

state:
| state_kind STATE IDENTIFIER LEFTTUBORG transitions RIGHTTUBORG  {{ kind = $1; name = State $3; transitions = $5 }}
;

state_kind:
| START      { Start }
| FINAL      { Final }
| STARTFINAL { StartFinal }
|            { Normal }     // Empty means that there is no State Kind
;

variables:
| { [] }
| variable variables { $1 :: $2 }
;

variable:
| HASH VAR id = IDENTIFIER EQUAL n = INT { Var_decl (id, int_of_string n) }
;

inputs:
| { Input_decl [] }
| INPUT identifier_list { Input_decl $2 }
;

identifier_list:
| id = event_name { [id] }
| id = event_name identifier_list { id :: $2 }

transitions:
| { [] }
| transition transitions    { $1 :: $2 }
;

transition:
| ON event_name GO IDENTIFIER operation_block_opt
    { Transition (Event $2, None, State $4, $5) }
| ON event_name IF expr GO IDENTIFIER operation_block_opt
    { Transition (Event $2, Some $4, State $6, $7) }
| AUTO GO IDENTIFIER operation_block_opt
    { Transition (Auto, None, State $3, $4) }
| AUTO IF expr GO IDENTIFIER operation_block_opt
    { Transition (Auto, Some $3, State $5, $6) }
;

event_name:
| IDENTIFIER { $1 }
| INT { $1 }

operation_block_opt:
  | { [] }
  | LEFTTUBORG operations RIGHTTUBORG { $2 }
  ;

operations:
  | operation { [$1] }    
  | operation operations { $1 :: $2 }
  ;

operation:
  | DO ident EQUAL expr { Do ($2, $4) }
  ; 

expr:
  | id = ident
      { Eident id }
  | INT
      { Ecst (Cint (int_of_string $1)) }
  | e1 = expr o = binop e2 = expr
      { Ebinop (o, e1, e2) }
  | LP e = expr RP
      { e }
;

%inline binop:
  | PLUS    { Badd }
  | MINUS   { Bsub }
  | TIMES   { Bmul }
  | LT      { Blt }
  | LTE     { Ble }
  | GT      { Bgt }
  | GTE     { Bge }
  | BEQUAL  { Beq }
  | BNEQUAL { Bneq }
  | AND     { Band }
  | OR      { Bor }
;

 
ident:
  IDENTIFIER { { loc = ($startpos, $endpos); id = $1 } }
;