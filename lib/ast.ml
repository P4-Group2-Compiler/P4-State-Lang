  

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
 | Event of string


type transition =
 | Transition of event * string


type state = {
    name : string; (*state B {}*) (*= State of string*)
    transitions : transition list; (*state B{ON open GO A}*)
}






type program = {
  states : state list;
}