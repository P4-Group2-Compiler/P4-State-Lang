(* Mega pasted from WHILE language, seems like we need to make a mini-arith-while language
if we want guards to work with generic expressions and not tailor made state guards *)



type location = Lexing.position * Lexing.position
type ident = { loc: location; id: string; }

(********** BOOLEAN SHENANIGANS IN RELATION TO GUARDS*********)
(*******************EG: ON EVENT IF (3 < 4) GO B****************************)
(* Unary operators. *)


(* Binary operators. *)
type binop =
  | Badd    (* + - * // % *)
  (*| Beq | Bneq *)
  | Blt (*| Ble | Bgt | Bge*)  (* == != < <= > >= *)
  (*| Band | Bor*)                          (* and or *)

(* Constants. *)
type constant =
  | Cbool of bool
  | Cstring of string
  | Cint of int

(* Expressions. *)
type expr =
  | Ecst of constant                   (* constant *)
  | Ebinop of binop * expr * expr      (* binary operation *)
  | Eident of ident                    (* variable *)                  

(* Statements. *)
type stmt =
  | Stmt_if of expr * stmt * stmt       (* conditional *)
  
type event =
  | Event of string (* Might be better to have simply 'type event = string' *)

type state =
  | State of string (* Might be better to have simply 'type state = string' *)

type transition =
  | Transition of event * expr option * state
(*| GuardTrans of event * expr * state *)

type state_kind =
  | Normal
  | Start
  | Final

type state_decl = {
  kind : state_kind;
  name : state;                     (*state B {}*) (*= State of string*)
  transitions : transition list;    (*state B{ON open GO A}*)
}

type program = {
  machine_name : string;
  states : state_decl list;
}