open Ast

exception Semantic_error of string

let error msg = raise (Semantic_error msg)

type transition = state * event * state

let event_to_string (Event e) = e (*Base printer for event*)
let state_to_string (State s) = s (*Base printer for state*)

let collect_states (p : program) : state list =
  List.map (fun state_decl -> state_decl.name) p.states

(* Function for adding the source state with the states: (event, state) -> (state, event, state) *)
let add_source_state (st_decl : state_decl) : transition list = 
  let src = st_decl.name in
  List.map (fun trans -> 
    match trans with
    | Transition (event, dest) -> (src, event, dest))
  st_decl.transitions

(* Collecting all the transitions using the add_source_state function: List of all (state, event, state)*)
let collect_transitions (p : program) : transition list = 
  List.concat (List.map add_source_state p.states)

(*---------------------------------------------------------------------------------------------------------------------------*)

(*Map for applying the collect_transitions*)
let rec lstColTrans f = function
| [] -> []
| h :: t -> f h :: lstColTrans f t

(*Creating the list of transitions, WITH the source states (list of lists)*)
let transList lst = lstColTrans (collect_transitions) lst