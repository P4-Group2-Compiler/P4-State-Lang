

type state = 
  | State of string
  | StartState of string
  | FinalState of string

(* TO BE ADDED
  (* Maybe change type name to 'event', depends what statements (if any) we add *)
type stmt =
  | Event of string *)


(* TO BE ADDED
type expr =
  | Identifier of string
  | Transition of state * stmt * state (* (State, Event -> State') *) *)

type program = 
  state