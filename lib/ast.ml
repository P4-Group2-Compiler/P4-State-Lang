  (*type state = 
  (*| State of string *)
  | State of string*)

(*type binstate =
  | START of state
  | FINAL of state*)


(*
type start =
  | Start of string
type final =
  | Final of string*)


(* TO BE ADDED
  (* Maybe change type name to 'event', depends what statements (if any) we add *)
type stmt =
  | Event of string *)


(* TO BE ADDED
type expr =
  | Identifier of string
  | Transition of state * stmt * state (* (State, Event -> State') *) *)

type event =
  | Event of string (* Might be better to have simply 'type event = string' *)

type state =
  | State of string (* Might be better to have simply 'type state = string' *)

type transition =
  | Transition of event * state

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