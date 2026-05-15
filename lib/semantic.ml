open Ast

(*------------------------------(* ERROR HANDLING CREATION *)-----------------------------*)
exception Semantic_error of string
let error msg = raise (Semantic_error msg)

(*-------------------------------------(* WARNINGS *)-------------------------------------*)
type warning = 
  | DuplicateTransitions of state * event * state
  | TooManyStates of int
  | UnreachableFinalState of state
  | NoFinalState

let maxStates = 20 (* Num of states that break the DOT output readability
                      Used for TooManyStates warning *)

(*------------------------(* DEFINING TYPE AND HELPER FUNCTIONS *)------------------------*)

type statemachine = 
{
  statemachine_name : string;
  states : state list;
  start_state : state;
  final_state : state list;
  transitions : (state * event * expr option * state * operation list) list;
  g_variables : var_decl list;
}

let state_to_string = function
  | State s -> s
let event_to_string = function
  | Event e -> e

(*********************************************************************************************************)
(*                                          STRING PRINTERS                                              *)
(*********************************************************************************************************)
let event_to_string (Event e) = e (*Base printer for event*)
let state_to_string (State s) = s (*Base printer for state*)

let binop_to_string = function
  | Badd -> "+" | Bsub -> "-"  | Bmul -> "*" | Bdiv -> "/" | Bmod -> "%" 
  | Beq -> "==" | Bneq -> "!=" | Blt -> "<"  | Ble -> "<=" | Bgt -> ">"  
  | Bge -> ">=" | Band -> "AND"| Bor -> "OR" 

let var_to_string (v : var_decl) =
  match v with
  | Var_decl (id, value) -> id ^ " = " ^ string_of_int value

let constant_to_string (c : constant) = 
  let const_string =
    match c with
    | Cbool c -> string_of_bool c
    | Cint c -> string_of_int c
  in
  const_string

let ident_to_string (i : ident) =
  i.id

let rec expr_to_string (e : expr) =
  let expr_string =
    match e with
    | Ecst e -> constant_to_string e
    | Ebinop (b, e1, e2) -> 
        Printf.sprintf "(%s %s %s)"
        (expr_to_string e1)
        (binop_to_string b)
        (expr_to_string e2)
    | Eident e -> ident_to_string e (*idk what this is useful for, but it's here*)
  in
  expr_string

let expr_option_to_string = function
  | None -> ""
  | Some expr -> expr_to_string expr
  
let rec stmt_to_string (s : stmt) =
  let stmt_string =
    match s with
    | Stmt_if (e, s1, s2) ->
      Printf.sprintf "(%s %s %s)"
      (expr_to_string e)
      (stmt_to_string s1)
      (stmt_to_string s2)
  in
  stmt_string

let op_to_string (o : operation) =
  let op_string =
    match o with
    | Do (id, expr) -> Printf.sprintf "%s" (ident_to_string id ^ " = " ^ expr_to_string expr)
  in  
  op_string

(*********************************************************************************************************)

let collect_states (p : program) : state list =
  List.map (fun state_decl -> state_decl.name) p.states




(* Get a list of all start states then checks the list to see if there is more than one *)
(* TODO: Fix the if statement in the end of this function, might mess up later development *)
let get_start_states (p: program) : state =
  let start_states = 
    List.fold_left (fun list state_decl -> 
      match state_decl.kind with
      | Start | StartFinal -> state_decl.name :: list
      | Normal | Final -> list)
      []
      p.states
    in
    if List.length start_states > 1 then error "Multiple Start states declared"
      else if List.length start_states = 0 then error "Missing Start state declaration"
      else List.hd start_states

(* Get a list of all Finals states in the program *)
let get_final_states (p: program) : state list = 
  List.fold_left (fun list state_decl ->
    match state_decl.kind with
    | Final | StartFinal -> state_decl.name :: list
    | Normal | Start -> list)
    []
    p.states

(* Function for adding the source state with the states: (event, state) -> (state, event, state) *)
let add_source_state (st_decl : state_decl) : (state * event * expr option * state * operation list) list = 
  let src = st_decl.name in
  List.map (fun trans -> 
    match trans with
    | Transition (event, Some expr, dest, ops) -> (src, event, Some expr, dest, ops)
    | Transition (event, None, dest, ops) -> (src, event, None, dest, ops))
st_decl.transitions

(* Collecting all the transitions using the add_source_state function: List of all (state, event, state)*)
let collect_transitions (p : program) : (state * event * expr option * state * operation list) list = 
  List.concat (List.map add_source_state p.states)

let collect_g_variables (p : program) : (var_decl) list =
  p.variables

(*----------------(* CREATING THE STATEMACHIN WITH THE HELPER FUNCTIONS *)----------------*)

(* Function to create the mathmatical StateMachine *)
let create_state_machine (p: program) : statemachine =
  {
    statemachine_name = p.machine_name;
    states = collect_states p;
    start_state = get_start_states p;
    final_state = get_final_states p;
    transitions = collect_transitions p;
    g_variables = collect_g_variables p;
  }

(*----------------------------(* VALIDATING THE STATEMACHINE *)---------------------------*)

let check_number_of_states (statemachine : statemachine) : warning list =
  let stateNum = List.length statemachine.states in
  if stateNum > maxStates then [TooManyStates (stateNum)]
  else []

let check_valid_transition (statemachine : statemachine) : unit =
  List.iter (fun (src, _, _, dest, _) -> 
    if not (List.mem dest statemachine.states)
      then error (Printf.sprintf
        "Transition {FROM: %s TO: %s} is invalid.\n\t%s is not declared as a State"
        (state_to_string src)
        (state_to_string dest)
        (state_to_string dest))
    else 
      () )
  statemachine.transitions

(* Recursive function to check if state names are repeated *)
let check_duplicate_state_names (statemachine : statemachine) : unit =
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
      let (src, event, _, dest, _) = head in
      if List.mem head seen then
        checker seen (DuplicateTransitions (src, event, dest) :: warnings) tail
      else
        checker (head :: seen) warnings tail
  in checker [] [] statemachine.transitions

(* Helper function for checking if start can reach final. It returns a list of destination states connected
   with transitions from the passed state *)
let successors (statemachine : statemachine) (state : state) : state list =
  List.fold_left (fun acc (src, _, _, dest, _) ->
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
let check_start_reaches_final (statemachine : statemachine) : warning list =
  let start = statemachine.start_state in
  if List.length statemachine.final_state = 0 then [NoFinalState]
  else if not (can_reach_final_state statemachine [] start) then
      [UnreachableFinalState start]
  else []

(* Returns a list of states that cannot reach Final state (dead-ends) might use for warnings later? *)
let get_dead_end_states (statemachine : statemachine) : state list =
  List.filter
    (fun state ->
      not (List.mem state statemachine.final_state) &&
      not (can_reach_final_state statemachine [] state))
    statemachine.states

(* Function to run through all of the validation checks - add all new checks into this function *)
let validate_state_machine (statemachine : statemachine) : unit =
  check_duplicate_state_names statemachine;
  check_valid_transition statemachine

let collect_warnings (statemachine : statemachine) : warning list =
  let warnings = [] in
    warnings
    @ (check_duplicate_transistions statemachine)
    @ (check_number_of_states statemachine)
    @ (check_start_reaches_final statemachine)

  (* let TooManyStates = check_number_of_states statemachine; *)

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
