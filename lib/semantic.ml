open Ast

exception Semantic_error of string

let error msg = raise (Semantic_error msg)

(*
let state_to_string = function
  | State s -> s
let event_to_string = function
  | Event e -> e
*)

(*Silke - "Mine funktioner i ./dottest bruger de to nederste og ikke de øverste, ved ikke med jer andre."*)
let event_to_string (Event e) = e (*Base printer for event*)
let state_to_string (State s) = s (*Base printer for state*)

let collect_states (p : program) : state list =
  List.map (fun state_decl -> state_decl.name) p.states


(* TODO: Fix the if statement in the end of this function, might mess up later development *)
let get_start_sates (p: program) : state option =
  let start_states = 
    List.fold_left (fun list state_decl -> 
      match state_decl.kind with
      | Start -> state_decl.name :: list
      | Normal | Final -> list)
      []
      p.states
    in
    if List.length start_states > 1 then error "Multiple Start states declared" else None

(* Function for adding the source state with the states: (event, state) -> (state, event, state) *)
let add_source_state (st_decl : state_decl) : (state * event * state) list = 
  let src = st_decl.name in
  List.map (fun trans -> 
    match trans with
    | Transition (event, dest) -> (src, event, dest))
  st_decl.transitions

(* Collecting all the transitions using the add_source_state function: List of all (state, event, state)*)
let collect_transitions (p : program) : (state * event * state) list = 
  List.concat (List.map add_source_state p.states)