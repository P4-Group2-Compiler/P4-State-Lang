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
(*%token ELSE
%token ELIF*)

(* Operators *)
%token (* BEQUAL BNEQUAL*) LTE GT GTE LT  
%token PLUS MINUS TIMES DIV MOD

// Punctuators
%token LEFTTUBORG RIGHTTUBORG (* '{' and '}' *)
%token LP RP (* COMMA *) EQUAL (* "("     ")"     ","     "="  *)
(*%token PRINT*)

%left PLUS
%nonassoc LT

// Grammatical starting point
%start prog

// Types returned by the AST
%type <Ast.program> prog
%type <Ast.expr> expr

%%

// Grammar Rules

prog:
| STATEMACHINE IDENTIFIER LEFTTUBORG variables states RIGHTTUBORG EOF  { {machine_name = $2; variables = $4; states = $5} }
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

  | LP e = expr RP
      { e }
;

transitions:
  {[]}
| transition transitions    { $1 :: $2 }
;

transition:
| ON IDENTIFIER GO IDENTIFIER
    { Transition (Event $2, None, State $4) }
| ON IDENTIFIER IF expr GO IDENTIFIER
    { Transition (Event $2, Some $4, State $6) }
(*| ON IDENTIFIER IF expr GO IDENTIFIER ELSE GO IDENTIFIER*)
(*| ON IDENTIFIER IF expr GO IDENTIFIER ELSE expr GO IDENTIFIER*)
(*| ON IDENTIFIER IF expr GO IDENTIFIER ELSE stmt GO IDENTIFIER*)
(*| ON IDENTIFIER IF expr GO IDENTIFIER ELIF expr GO IDENTIFIER ELSE*)
;



(*simple_stmt:
| id = ident EQUAL e = expr
    { Sassign (id, e) }
| id = ident PLUS EQUAL e = expr
    { Sassign (id, Ebinop (Badd, Eident id, e)) }
| PRINT LP el = separated_list(COMMA, expr) RP
    { Sprint el }
;*)

(*binop:
| PLUS           { Badd }
(*| MINUS        { Bsub }
| TIMES          { Bmul }
| DIV            { Bdiv }
| MOD            { Bmod }
| BEQUAL         { Beq }
| BNEQUAL        { Bneq } *)
| LT             { Blt }
(*| LTE          { Ble }
| GT             { Bgt }
| GTE            { Bge }*)*)
;

 
ident:
  IDENTIFIER { { loc = ($startpos, $endpos); id = $1 } }
;