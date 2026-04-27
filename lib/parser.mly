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
%token <string> IDENT

(* Keywords *)
%token STATEMACHINE
%token STATE
%token START
%token FINAL
%token ON
%token GO
%token IF
%token ELSE
%token ELIF

(* Operators *)
%token BEQUAL BNEQUAL LT LTE GT GTE 
%token PLUS MINUS TIMES DIV MOD

// Punctuators
%token LEFTTUBORG RIGHTTUBORG (* '{' and '}' *)
%token LP RP COMMA EQUAL (* "("     ")"     ","     "="  *)
%token PRINT

%nonassoc LT

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

expr:
  | IDENTIFIER
      { let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos) in
        Eident { loc = dummy_loc; id = $1 } }
  | INT
      { Ecst (Cint (int_of_string $1)) }
  | e1 = expr o = binop e2 = expr
      { Ebinop (o, e1, e2) }
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



simple_stmt:
| id = ident EQUAL e = expr
    { Sassign (id, e) }
| id = ident PLUS EQUAL e = expr
    { Sassign (id, Ebinop (Badd, Eident id, e)) }
| PRINT LP el = separated_list(COMMA, expr) RP
    { Sprint el }
;

binop:
| PLUS           { Badd }
(*| MINUS          { Bsub }
| TIMES          { Bmul }
| DIV            { Bdiv }
| MOD            { Bmod }
| BEQUAL         { Beq }
| BNEQUAL        { Bneq } *)
| LT             { Blt }
(*| LTE            { Ble }
| GT             { Bgt }
| GTE            { Bge }*)
;

 
ident:
  IDENTIFIER { { loc = ($startpos, $endpos); id = $1 } }
