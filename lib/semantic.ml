open Ast

(*------------------------------(* ERROR HANDLING CREATION *)-----------------------------*)
exception Semantic_error of string

let error msg = raise (Semantic_error msg)


(*------------------------(* DEFINING TYPE AND HELPER FUNCTIONS *)------------------------*)

type statemachine = 
{
  statemachine_name : string;
  states : state list;
  start_state : state option;
  final_state : state list;
  transitions : (state * event * state) list;
}

let state_to_string = function
  | State s -> s
let event_to_string = function
  | Event e -> e

let event_to_string (Event e) = e (*Base printer for event*)
let state_to_string (State s) = s (*Base printer for state*)

let collect_states (p : program) : state list =
  List.map (fun state_decl -> state_decl.name) p.states


(* Get a list of all start states then checks the list to see if there is more than one *)
(* TODO: Fix the if statement in the end of this function, might mess up later development *)
let get_start_states (p: program) : state option =
  let start_states = 
    List.fold_left (fun list state_decl -> 
      match state_decl.kind with
      | Start -> state_decl.name :: list
      | Normal | Final -> list)
      []
      p.states
    in
    if List.length start_states > 1 then error "Multiple Start states declared"
      else if List.length start_states = 0 then error "Missing Start state decleration" else None 

(* Get a list of all Finals states in the program *)
let get_final_states (p: program) : state list = 
  List.fold_left (fun list state_decl ->
    match state_decl.kind with
    |Final -> state_decl.name :: list
    | Normal | Start -> list)
    []
    p.states

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


(*----------------(* CREATING THE STATEMACHIN WITH THE HELPER FUNCTIONS *)----------------*)

(* Function to create the mathmatical StateMachine *)
let create_state_machine (p: program) : statemachine =
  {
    statemachine_name = p.machine_name;
    states = collect_states p;
    start_state = get_start_states p;
    final_state = get_final_states p;
    transitions = collect_transitions p;
  }


(*----------------------------(* VALIDATING THE STATEMACHINE *)---------------------------*)

(* Recursive function to check if state names are repeated *)
let check_duplicate_state_names (statemachine: statemachine) : unit =
  let rec checker seen = function
    | [] -> ()
    | head :: tail ->
      let name = state_to_string head in
      if List.mem name seen then
        error "Duplicate state names declared" 
      else
        checker (name :: seen) tail
  in checker [] statemachine.states

let check_duplicate_transistions (statemachine : statemachine) : unit =
  let rec checker seen = function
    | [] -> ()
    | head :: tail ->
      if List.mem head seen then
        error "Dulicate transition declared"
      else
        checker (head :: seen) tail
  in checker [] statemachine.transitions


(* Function to run through all of the validation checks - add all new checks into this function *)
let validate_state_machine (statemachine : statemachine) : unit =
  check_duplicate_state_names statemachine;
  check_duplicate_transistions statemachine


(*---------------------------(* ANALYSE THE STATEMACHINE *)---------------------------*)

(* This function is the one called by other files. It build the statemachine and runs through all the
   checks. The statemachine that is returned is the one used for codegen *)
let analyse (p : program) : statemachine =
  let machine = create_state_machine p in
      validate_state_machine machine;
      machine


(*---------------------------(* PRINT FUNCTIONS FOR DEBUGGIN *)---------------------------*)

(* Un-comment those that you need when testing *)

(* let print_transition (src, event, dest) =
  Printf.printf "%s --%s--> %s\n" 
  (state_to_string src)
  (event_to_string event)
  (state_to_string dest)

let print_iter_trans (t: (state * event * state) list) = 
  List.iter print_transition t; *)
