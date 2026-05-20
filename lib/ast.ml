
(* Information passed from Lexer to include position tracking for errors *)
type location = Lexing.position * Lexing.position
type ident = { loc: location; id: string; }

(* ******************************************************************************** *) 
(*  BOOLEAN DEFINITIONS IN RELATION TO GUARDS --- EG: ON EVENT IF (x < 4) GO STATE  *)
(* ******************************************************************************** *)

(* Binary operators. *)
type binop =
  | Badd | Bsub | Bmul   (* + - * *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor  (* and or *)

(* Constants. *)
type constant =
  | Cbool of bool
  | Cint of int

(* Expressions. *)
type expr =
  | Ecst of constant                   (* constant *)
  | Ebinop of binop * expr * expr      (* binary operation *)
  | Eident of ident                    (* variable *)

(* Statements. *)
type stmt =
  | Stmt_if of expr * stmt * stmt       (* conditional *)

(* ************************************************************************** *) 
(*                          STATEMACHINE RELATED TYPES                        *)
(* ************************************************************************** *)

type event =
  | Event of string
  | Auto

type state =
  | State of string

type operation =
  | Do of ident * expr

type transition =
  | Transition of event * expr option * state * operation list

type state_kind =
  | Normal
  | Start
  | Final
  | StartFinal

type var_decl = 
  | Var_decl of string * int

type input_decl =
  | Input_decl of string list

type state_decl = {
  kind : state_kind;
  name : state;                     (*state B {}*) (*= State of string*)
  transitions : transition list;    (*state B{ON open GO A}*)
}

type program = {
  machine_name : string;
  variables : var_decl list;
  inputs : input_decl;
  states : state_decl list;
}