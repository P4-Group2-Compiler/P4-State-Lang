open Ast

(*------------------------------(* ERROR HANDLING CREATION *)-----------------------------*)
exception Semantic_error of string

let error msg = raise (Semantic_error msg)

(*-------------------------------------(* WARNINGS *)-------------------------------------*)
type warning = 
  | DuplicateTransitions of state * event * state


(*------------------------(* DEFINING TYPE AND HELPER FUNCTIONS *)------------------------*)

type statemachine = 
{
  statemachine_name : string;
  states : state list;
  start_state : state;
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
let get_start_states (p: program) : state =
  let start_states = 
    List.fold_left (fun list state_decl -> 
      match state_decl.kind with
      | Start -> state_decl.name :: list
      | Normal | Final -> list)
      []
      p.states
    in
    if List.length start_states > 1 then error "Multiple Start states declared"
      else if List.length start_states = 0 then error "Missing Start state decleration"
      else List.hd start_states

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

  (* Recursive function that checks if the same transition has been declared more than once *)
let check_duplicate_transistions (statemachine : statemachine) : warning list =
  let rec checker seen warnings = function
    | [] -> warnings
    | head :: tail ->
      let (src, event, dest) = head in
      if List.mem head seen then
        checker seen (DuplicateTransitions (src, event, dest) :: warnings) tail
      else
        checker (head :: seen) warnings tail
  in checker [] [] statemachine.transitions

(* Helper function for checking if start can reach final. It returns a list of destination states connected
   with transitions from the passed state *)
let successors (statemachine : statemachine) (state : state) : state list =
  List.fold_left (fun acc (src, _, dest) ->
    if src = state then dest :: acc else acc)
  [] statemachine.transitions

(* Helper function for the check_start_reaches_final *)
let rec can_reach_final_state (statemachine : statemachine) (visited : state list) (state : state) : bool =
  if List.mem state statemachine.final_state then true
  else if List.mem state visited then false
  else
    let next_states = successors statemachine state in
    List.exists (fun next -> can_reach_final_state statemachine (state :: visited) next) next_states

(* We check if the Start state has a path to the Final state *)
let check_start_reaches_final (statemachine : statemachine) : unit =
  let start = statemachine.start_state in  
  if not (can_reach_final_state statemachine [] start) then
      error ("{Start State " ^ state_to_string start ^ "} cannot reach a Final State")

(* Returns a list of states that cannot reach Final state (dead-ends) might use for warnings later? *)
let get_unreachable_states (statemachine : statemachine) : state list =
  List.filter
    (fun state ->
      not (List.mem state statemachine.final_state) &&
      not (can_reach_final_state statemachine [] state))
    statemachine.states

(* Function to run through all of the validation checks - add all new checks into this function *)
let validate_state_machine (statemachine : statemachine) : unit =
  check_duplicate_state_names statemachine;
  (* check_duplicate_transistions statemachine; *) (* Removed as it is now a warning *)
  check_start_reaches_final statemachine

let collect_warnings (statemachine : statemachine) : warning list =
  check_duplicate_transistions statemachine

(*---------------------------(* ANALYSE THE STATEMACHINE *)---------------------------*)

(* This function is the one called by other files. It build the statemachine and runs through all the
   checks. The statemachine that is returned is the one used for codegen *)
let analyse (p : program) : statemachine * warning list =
  let machine = create_state_machine p in
      validate_state_machine machine;
  let warnings = collect_warnings machine in
      (machine, warnings)


(*---------------------------(* PRINT FUNCTIONS FOR DEBUGGIN *)---------------------------*)

(* Un-comment those that you need when testing *)

(* let print_transition (src, event, dest) =
  Printf.printf "%s --%s--> %s\n" 
  (state_to_string src)
  (event_to_string event)
  (state_to_string dest)

let print_iter_trans (t: (state * event * state) list) = 
  List.iter print_transition t; *)
