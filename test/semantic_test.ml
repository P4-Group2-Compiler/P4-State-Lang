open P4
(* **************************************** *)
(* Tests:                                   *)
(* test_collect_states_happy                *)
(*                                          *) 
(* **************************************** *)

(* **************************************** *)
(*                                          *)
(*            FUN COLLECT_STATES            *)
(*                                          *)
(* **************************************** *)

(* **************************************** *)
(*               Happy test                 *)
(* **************************************** *)

let test_collect_states_happy () = 
  (* ARRANGE
    Use our AST and define a test input. We dont care for transitions,
    they are left empty intentionally *)
  let open P4.Ast in
  let state name =
    { kind = Normal; name = State name; transitions = [] }
  in
  (* The program is defined, some State Machine M, consisting of states "A" and "B" *)
  let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [ state "A"; state "B" ];
  } in

  (* ACT 
    We call the function that is being tested *)
  let result = P4.Semantic.collect_states prog in

  (* ASSERT 
    We assert statements. We expect the function to return a list with the states "A" and "B" *)
  let expected = [ State "A"; State "B" ] in
  Alcotest.(check (list string))
    "collect_states returns correct names"
    (List.map Semantic.state_to_string expected)
    (List.map Semantic.state_to_string result)

    
    
(* ******************************************************** *)
(*                                                          *)
(*                FUN COLLECT_TRANSITIONS                   *)
(*                                                          *)
(************************************************************)

(************************************************************)
(*                        HAPPY TEST                        *)
(************************************************************)
let test_collect_transitions_happy () =

let open P4.Ast in
let state name =
  { kind = Start; name = State name; transitions = [] }
in
let prog = {
  machine_name = "M";
  variables = [];
  inputs = [];
  states = [ state "A"]
} in

let result = P4.Semantic.get_start_states prog in

let expected = State "A" in
Alcotest.(check string)
  "get_start_state returns the correct start state"
  (P4.Semantic.state_to_string expected)
  (P4.Semantic.state_to_string result) 

(*************************************************************)
(*                        SAD TESTS                          *)
(*************************************************************)

let test_transitions_multiple_start_states () =

let open P4.Ast in
let state name =
    { kind = Start; name = State name; transitions = [] } in
let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [ state "A"; state "B" ] 
} in
Alcotest.check_raises
    "get_start_states raises error: 'Multiple Start states declared'"
    (P4.Semantic.Semantic_error "Multiple Start states declared")
    (fun () -> ignore (P4.Semantic.get_start_states prog))

let test_transition_no_start_state () =

let open P4.Ast in
let state name = 
    { kind = Normal; name = State name; transitions = [] } in
let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [ state "A"; state "B"]
 } in
 Alcotest.check_raises
    "get_start_states raises error: 'Missing Start state declared"
    (P4.Semantic.Semantic_error "Missing Start state declared")
    (fun () -> ignore (P4.Semantic.get_start_states prog))

(* ******************************************************** *)
(*                                                          *)
(*                   FUN GET_FINAL_STATES                   *)
(*                                                          *)
(* ******************************************************** *)

(************************************************************)
(*                        HAPPY TESTS                       *)
(************************************************************)

(* ******************************************************** *)
(*                                                          *)
(*   Initializing the tests. The de facto 'run' function    *)
(*                                                          *)
(* ******************************************************** *)

let () =
  Alcotest.run "Semantic Tests" [
    "semantic", [
      Alcotest.test_case "collect_states" `Quick test_collect_states_happy;
      Alcotest.test_case "collect_transitions" `Quick test_collect_transitions_happy;
      Alcotest.test_case "collect_transitions" `Quick test_transitions_multiple_start_states;
      ];
  ]