(* '%' is a menhir header for direct Ocaml injection of code *)

%{
    open Ast
%}

// Token Decleration

// WHILE lang tokens

// %token ELSE PRINT AND OR NOT


// WHILE lang tokens end

%token EOF

(* Identifiers and integers *)
%token <string> IDENTIFIER
%token <string> INT
(*%token <string> IDENT*)

(* Keywords *)
%token STATEMACHINE
%token STATE
%token START
%token FINAL
%token ON
%token GO
%token IF
%token HASH
%token VAR
%token AND
%token OR
%token INPUT
(*%token ELSE
%token ELIF*)
%token DO
%token AUTO

(* Operators *)
%token BEQUAL BNEQUAL LTE GT GTE LT  
%token PLUS MINUS TIMES DIV MOD

// Punctuators
%token LEFTTUBORG RIGHTTUBORG (* '{' and '}' *)
%token LP RP (* COMMA *) EQUAL (* "("     ")"     ","     "="  *)
(*%token PRINT*)

%left OR
%left AND
%nonassoc BEQUAL BNEQUAL LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIV MOD


// Grammatical starting point
%start prog

// Types returned by the AST
%type <Ast.program> prog
%type <Ast.expr> expr

%%

// Grammar Rules

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
| START { Start }
| FINAL { Final }
|       { Normal } // Empty means that there is no State Kind
;

variables:
| { [] }
| variable variables { $1 :: $2 }
;

variable:
| HASH VAR id = IDENTIFIER EQUAL n = INT { Var_decl (id, int_of_string n) }
;

inputs:
| { [] }
| input inputs { $1 :: $2 }
;

input:
| INPUT identifier_list { Input_decl $2 }

identifier_list:
| id = IDENTIFIER { [id] }
| id = IDENTIFIER identifier_list { id :: $2 }

transitions:
| { [] }
| transition transitions    { $1 :: $2 }
;

transition:
| ON IDENTIFIER GO IDENTIFIER operation_block_opt
    { Transition (Event $2, None, State $4, $5) }
| ON IDENTIFIER IF expr GO IDENTIFIER operation_block_opt
    { Transition (Event $2, Some $4, State $6, $7) }
| AUTO GO IDENTIFIER operation_block_opt
    { Transition (Auto, None, State $3, $4) }
| AUTO IF expr GO IDENTIFIER operation_block_opt
    { Transition (Auto, Some $3, State $5, $6) }
(*| ON IDENTIFIER IF expr GO IDENTIFIER ELSE GO IDENTIFIER*)
(*| ON IDENTIFIER IF expr GO IDENTIFIER ELSE expr GO IDENTIFIER*)
(*| ON IDENTIFIER IF expr GO IDENTIFIER ELSE stmt GO IDENTIFIER*)
(*| ON IDENTIFIER IF expr GO IDENTIFIER ELIF expr GO IDENTIFIER ELSE*)
;

operation_block_opt:
  | { [] }
  | LEFTTUBORG operations RIGHTTUBORG { $2 }
  ;

operations:
  | operation { [$1] }    
  | operation operations { $1 :: $2 }
  ;

operation:
  | DO expr { Do $2 }  

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

(*
expr:
  | id = ident          
      { Eident id }
  | INT
      { Ecst (Cint (int_of_string $1)) }
  | e1 = expr PLUS e2 = expr
      { Ebinop (Badd, e1, e2) }
  | e1 = expr MINUS e2 = expr
      { Ebinop (Bsub, e1, e2) }
  | e1 = expr TIMES e2 = expr
      { Ebinop (Bmul, e1, e2) }
  | e1 = expr DIV e2 = expr
      { Ebinop (Bdiv, e1, e2) }
  | e1 = expr MOD e2 = expr
      { Ebinop (Bmod, e1, e2) }
  | e1 = expr LT e2 = expr
      { Ebinop (Blt, e1, e2) }
  | e1 = expr LTE e2 = expr
      { Ebinop (Ble, e1, e2) }
  | e1 = expr GT e2 = expr
      { Ebinop (Bgt, e1, e2) }
  | e1 = expr GTE e2 = expr
      { Ebinop (Bge, e1, e2) }
  | e1 = expr AND e2 = expr 
    { Ebinop (Band, e1, e2) }
  | e1 = expr OR  e2 = expr 
    { Ebinop (Bor,  e1, e2) }
  | e1 = expr BEQUAL  e2 = expr 
    { Ebinop (Beq,  e1, e2) }
  | e1 = expr BNEQUAL  e2 = expr 
    { Ebinop (Bneq,  e1, e2) }

  | LP e = expr RP
      { e }
;
*)



(*simple_stmt:
| id = ident EQUAL e = expr
    { Sassign (id, e) }
| id = ident PLUS EQUAL e = expr
    { Sassign (id, Ebinop (Badd, Eident id, e)) }
| PRINT LP el = separated_list(COMMA, expr) RP
    { Sprint el }
;*)

%inline binop:
  | PLUS    { Badd }
  | MINUS   { Bsub }
  | TIMES   { Bmul }
  | DIV     { Bdiv }
  | MOD     { Bmod }
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