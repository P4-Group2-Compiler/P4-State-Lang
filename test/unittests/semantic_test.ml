open P4
open Test_helper
(* ******************************************************** *)
(*                   FUN COLLECT_STATES                     *)
(* ******************************************************** *)

(* ******************************************************** *)
(*                        HAPPY TEST                        *)
(* ******************************************************** *)
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
(*                  FUN GET_START_STATES                    *)
(* ******************************************************** *)

(* ******************************************************** *)
(*                        HAPPY TEST                        *)
(* ******************************************************** *)

let test_get_start_states_start_happy () =
  let open P4.Ast in
  let open Test_helper in

  let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Start; name = State "A"; transitions = [] };
        { kind = Final; name = State "B"; transitions = [] }
      ]
  } in

  let result = P4.Semantic.get_start_states prog in

  let expected = State "A" in

  Alcotest.(check state_testable)
    "get_start_states returns correct start state"
    expected
    result

let test_get_start_states_startfinal_happy () =
  let open P4.Ast in
  let open Test_helper in

  let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = StartFinal; name = State "A"; transitions = [] };
        { kind = Final; name = State "B"; transitions = [] }
      ]
  } in

  let result = P4.Semantic.get_start_states prog in

  let expected = State "A" in

  Alcotest.(check state_testable)
    "get_start_states returns correct start state"
    expected
    result

(* ******************************************************** *)
(*                         SAD TEST                         *)
(* ******************************************************** *)

let test_get_start_states_multiple_start_states () =

let open P4.Ast in
let start_state name =
    { kind = Start; name = State name; transitions = [] } in
let startfinal_state name =
    { kind = StartFinal; name = State name; transitions = [] } in
    let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [ start_state "A"; startfinal_state "B" ]
} in
Alcotest.check_raises
    "get_start_states raises error: 'Multiple Start states declared'"
    (P4.Semantic.Semantic_error "Multiple Start states declared")
    (fun () -> ignore (P4.Semantic.get_start_states prog))


let test_get_start_states_no_start_state () =

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
    "get_start_states raises error: 'Missing Start state declaration"
    (P4.Semantic.Semantic_error "Missing Start state declaration")
    (fun () -> ignore (P4.Semantic.get_start_states prog))

(* ******************************************************** *)
(*                   FUN GET_FINAL_STATES                   *)
(* ******************************************************** *)

(* ******************************************************** *)
(*                        HAPPY TEST                        *)
(* ******************************************************** *)

let test_get_final_states_happy () =
  let open P4.Ast in

  let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Final; name = State "A"; transitions = [] };
        { kind = StartFinal; name = State "B"; transitions = [] };
        { kind = Start; name = State "C"; transitions = [] }
        ]
  } in
    let result = P4.Semantic.get_final_states prog in 

    let expected = [ State "B"; State "A" ] in
    Alcotest.(check (list string))
      "collect_states returns correct names"
      (List.map Semantic.state_to_string expected)
      (List.map Semantic.state_to_string result) 
    
(* *********************************************************** *)
(*                   FUN COLLECT_TRANSITIONS                   *)
(* *********************************************************** *)

(* *********************************************************** *)
(*                         HAPPY TEST                          *)
(* *********************************************************** *)
let test_collect_transitions_happy () =
  let open P4.Ast in
  let open Test_helper in
  let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [
      { kind = Start; name = State "A"; transitions = [
          Transition (Event "e1", None, State "B", [])
        ]};
      { kind = Normal; name = State "B"; transitions = [
          Transition (Event "e2", None, State "A", [])
        ]};
    ]
  } in

  let result = P4.Semantic.collect_transitions prog in

  let expected = [
    (State "A", Event "e1", None, State "B", []);
    (State "B", Event "e2", None, State "A", []);
  ] in

  Alcotest.(check (list transition_testable))
    "collect_transitions returns correct transitions"
    expected
    result

(* *********************************************************** *)
(*                   FUN COLLECT_G_VARIABLES                   *)
(* *********************************************************** *)

(* *********************************************************** *)
(*                         HAPPY TEST                          *)
(* *********************************************************** *)

let test_collect_g_variables_happy () =
  let open P4.Ast in
  let open Test_helper in

  let prog = {
    machine_name = "M";
    variables = [
      Var_decl ("x", 2);
      Var_decl ("y", 3);
      Var_decl ("incomeTax", 456)
    ];
    inputs = [];
    states = [];
  } in

  let result = P4.Semantic.collect_g_variables prog in

  let expected = [
    Var_decl ("x", 2);
    Var_decl ("y", 3);
    Var_decl ("incomeTax", 456)
  ] in

  Alcotest.(check var_decl_list_testable)
    "collect_g_variables returns correct variables"
    expected
    result

(* *********************************************************** *)
(*                 FUN CHECK_NUMBER_OF_STATES                  *)
(* *********************************************************** *)

(* *********************************************************** *)
(*                         HAPPY TEST                          *)
(* *********************************************************** *)

let test_check_number_of_states_happy () =
  let open P4.Ast in
  let open Test_helper in
  let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Final; name = State "A"; transitions = [] };
        { kind = Final; name = State "B"; transitions = [] };
        { kind = Start; name = State "C"; transitions = [] }
      ]
  } in
    let sm = P4.Semantic.create_state_machine prog in
    let result = P4.Semantic.check_number_of_states sm in

    Alcotest.(check (list warning_testable))
      "No warning when state count is within limit"
      []
      result

(* *********************************************************** *)
(*                          SAD TEST                           *)
(* *********************************************************** *)

let test_check_number_of_states_sad () =
    let open P4.Ast in
    let open Test_helper in
    let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Final; name = State "A"; transitions = [] };
        { kind = Normal; name = State "B"; transitions = [] };
        { kind = Normal; name = State "C"; transitions = [] };
        { kind = Normal; name = State "D"; transitions = [] };
        { kind = Normal; name = State "E"; transitions = [] };
        { kind = Start; name = State "F"; transitions = [] };
        { kind = Normal; name = State "G"; transitions = [] };
        { kind = Normal; name = State "H"; transitions = [] };
        { kind = Normal; name = State "I"; transitions = [] };
        { kind = Normal; name = State "J"; transitions = [] };
        { kind = Normal; name = State "K"; transitions = [] };
        { kind = Final; name = State "L"; transitions = [] };
        { kind = Normal; name = State "M"; transitions = [] };
        { kind = Normal; name = State "N"; transitions = [] };
        { kind = Normal; name = State "O"; transitions = [] };
        { kind = Normal; name = State "P"; transitions = [] };
        { kind = Normal; name = State "Q"; transitions = [] };
        { kind = Normal; name = State "R"; transitions = [] };
        { kind = Normal; name = State "S"; transitions = [] };
        { kind = Normal; name = State "T"; transitions = [] };
        { kind = Normal; name = State "U"; transitions = [] };
        { kind = Normal; name = State "V"; transitions = [] };
        ]

  } in
    let sm = P4.Semantic.create_state_machine prog in
    let result = P4.Semantic.check_number_of_states sm in

    Alcotest.(check (list warning_testable))
      "No warning when state count is within limit"
      [TooManyStates (22)]
      result

(* *********************************************************** *)
(*                 FUN CHECK_VALID_TRANSITION                  *)
(* *********************************************************** *)

(* *********************************************************** *)
(*                         HAPPY TEST                          *)
(* *********************************************************** *)

let test_check_valid_transition_happy () =
    let open P4.Ast in
    let open Test_helper in
    let transA = 
        [
        (Transition (Event "e1", None, State "B", []));
        (Transition (Event "e2", None, State "C", []))
        ] in
    let transB = 
        [
        (Transition (Event "e1", None, State "B", []));
        (Transition (Event "e2", None, State "A", []))
        ] in
    let transC = 
        [
        (Transition (Event "e1", None, State "A", []));
        (Transition (Event "e2", None, State "C", []))
        ] in
     
    let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Start; name = State "A"; transitions = transA};
        { kind = Normal; name = State "B"; transitions = transB};
        { kind = Final; name = State "C"; transitions = transC}
      ]
    } in

    let sm = P4.Semantic.create_state_machine prog in
    P4.Semantic.check_valid_transition sm

(* ********************************************************* *)
(*                         SAD TEST                          *)
(* ********************************************************* *)

let test_check_valid_transition_sad () =
    let open P4.Ast in
    let open Test_helper in
    let transA = 
        [
        (Transition (Event "e1", None, State "B", []));
        (Transition (Event "e2", None, State "C", []))
        ] in
    let transB = 
        [
        (Transition (Event "e1", None, State "B", []));
        (Transition (Event "e2", None, State "A", []))
        ] in
    let transC = 
        [
        (Transition (Event "e1", None, State "A", []));
        (Transition (Event "e2", None, State "Z", []))
        ] in
     
    let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Start; name = State "A"; transitions = transA};
        { kind = Normal; name = State "B"; transitions = transB};
        { kind = Final; name = State "C"; transitions = transC}
      ]
    } in

    let sm = P4.Semantic.create_state_machine prog in
    
    expect_semantic_error (fun () ->
    P4.Semantic.check_valid_transition sm
  )

(* *********************************************************** *)
(*               FUN CHECK_DUPLICATE_STATE_NAME                *)
(* *********************************************************** *)

(* *********************************************************** *)
(*                         HAPPY TEST                          *)
(* *********************************************************** *)

let test_check_duplicate_state_names_happy () =
  let open P4.Ast in
  let open Test_helper in
  let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Start; name = State "A"; transitions = []};
        { kind = Normal; name = State "B"; transitions = []};
        { kind = Final; name = State "C"; transitions = []}
      ]
    } in

  let sm = P4.Semantic.create_state_machine prog in
  P4.Semantic.check_duplicate_state_names sm

(* ********************************************************* *)
(*                         SAD TEST                          *)
(* ********************************************************* *)

let test_check_duplicate_state_names_sad () =
  let open P4.Ast in
  let open Test_helper in
  let prog = {
    machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Start; name = State "A"; transitions = []};
        { kind = Normal; name = State "A"; transitions = []};
        { kind = Final; name = State "C"; transitions = []}
      ]
    } in

  let sm = P4.Semantic.create_state_machine prog in
  
  expect_semantic_error (fun () ->
  P4.Semantic.check_duplicate_state_names sm)
    
(* *********************************************************** *)
(*               FUN CHECK_DUPLICATE_TRANSITIONS               *)
(* *********************************************************** *)

(* *********************************************************** *)
(*                         HAPPY TEST                          *)
(* *********************************************************** *)

let test_check_duplicate_transitions_happy () =
  let open P4.Ast in
  let open Test_helper in
  let transA = 
        [
        (Transition (Event "e1", None, State "B", []));
        (Transition (Event "e2", None, State "C", []))
        ] in
  let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Final; name = State "A"; transitions = transA };
        { kind = Final; name = State "B"; transitions = [] };
        { kind = Start; name = State "C"; transitions = [] }
      ]
  } in
    let sm = P4.Semantic.create_state_machine prog in
    let result = P4.Semantic.check_number_of_states sm in

    Alcotest.(check (list warning_testable))
      "No warning when no duplicate transition"
      []
      result

(* *********************************************************** *)
(*                          SAD TEST                           *)
(* *********************************************************** *)

let test_check_duplicate_transitions_sad () =
  let open P4.Ast in
  let open Test_helper in
  let transA = 
        [
        (Transition (Event "e1", None, State "B", []));
        (Transition (Event "e1", None, State "B", []))
        ] in
  let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Final; name = State "A"; transitions = transA };
        { kind = Final; name = State "B"; transitions = [] };
        { kind = Start; name = State "C"; transitions = [] }
      ]
  } in
    let sm = P4.Semantic.create_state_machine prog in
    let result = P4.Semantic.check_duplicate_transistions sm in
 
    Alcotest.(check (list warning_testable))
      "No warning when state count is within limit"
      [DuplicateTransitions (State "A", Event "e1", State "B")]
      result

(* *********************************************************** *)
(*                      FUN SUCCESSORS                         *)
(* *********************************************************** *)

(* *********************************************************** *)
(*                         HAPPY TEST                          *)
(* *********************************************************** *)

let test_successors_happy () =
  let open P4.Ast in
  let open Test_helper in
  let transA = 
        [
        (Transition (Event "e1", None, State "B", []));
        (Transition (Event "e2", None, State "C", []))
        ] in
    let transB = 
        [
        (Transition (Event "e1", None, State "B", []));
        (Transition (Event "e2", None, State "A", []))
        ] in
    let transC = 
        [
        (Transition (Event "e1", None, State "A", []));
        (Transition (Event "e2", None, State "C", []))
        ] in
     
    let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Start; name = State "A"; transitions = transA};
        { kind = Normal; name = State "B"; transitions = transB};
        { kind = Final; name = State "C"; transitions = transC}
      ]
    } in
    let sm = P4.Semantic.create_state_machine prog in
    
    let result_A = P4.Semantic.successors sm (State "A") in
    let expected_A = [State "C"; State "B"] in
    Alcotest.(check (list state_testable))
      "Successors of state A"
      expected_A
      result_A;
    
    let result_B = P4.Semantic.successors sm (State "B") in
    let expected_B = [State "A"; State "B"] in
    Alcotest.(check (list state_testable))
      "Successors of state B"
      expected_B
      result_B;  

    let result_C = P4.Semantic.successors sm (State "C") in
    let expected_C = [State "C"; State "A"] in
    Alcotest.(check (list state_testable))
      "Successors of state C"
      expected_C
      result_C

(* ********************************************************* *)
(*                         SAD TEST                          *)
(* ********************************************************* *)  

let test_successors_sad () =
  let open P4.Ast in
  let open Test_helper in
  let transA = 
        [] in
  let transB = 
      [
      (Transition (Event "e1", None, State "B", []));
      (Transition (Event "e2", None, State "A", []))
      ] in
  let transC = 
      [
      (Transition (Event "e1", None, State "A", []));
      (Transition (Event "e2", None, State "C", []))
      ] in
    
  let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [
      { kind = Start; name = State "A"; transitions = transA};
      { kind = Normal; name = State "B"; transitions = transB};
      { kind = Final; name = State "C"; transitions = transC}
    ]
  } in
  let sm = P4.Semantic.create_state_machine prog in
  
  let result_A = P4.Semantic.successors sm (State "A") in
  let expected_A = [] in
  Alcotest.(check (list state_testable))
    "Successors of state A"
    expected_A
    result_A

(* *********************************************************** *)
(*                  FUN CAN_REACH_FINAL_STATE                  *)
(* *********************************************************** *)

(* *********************************************************** *)
(*                         HAPPY TEST                          *)
(* *********************************************************** *)

let test_can_reach_final_state_happy () =
  let open P4.Ast in
  let open Test_helper in
  let transA = 
      [
      (Transition (Event "e1", None, State "B", []));
      ] in
  let transB = 
      [
      (Transition (Event "e2", None, State "C", []))
      ] in
  let transC = 
      [] in
    
  let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [
      { kind = Start; name = State "A"; transitions = transA};
      { kind = Normal; name = State "B"; transitions = transB};
      { kind = Final; name = State "C"; transitions = transC}
    ]
  } in
  
  let sm = P4.Semantic.create_state_machine prog in
  let result = P4.Semantic.can_reach_final_state sm [] (State "A") in
  let expected = true in
  Alcotest.(check (bool))
    "Start state A reaches the final state C"
  expected
  result

let test_can_reach_final_state_cycle_happy () =
    let open P4.Ast in
  let open Test_helper in
  let transA = 
      [
      (Transition (Event "e1", None, State "B", []));
      ] in
  let transB = 
      [
      (Transition (Event "e2", None, State "C", []))
      ] in
  let transC = 
      [
      (Transition (Event "e3", None, State "A", []));
      (Transition (Event "e4", None, State "D", []))
      ] in
  let transD =
      [] in
    
  let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [
      { kind = Start; name = State "A"; transitions = transA};
      { kind = Normal; name = State "B"; transitions = transB};
      { kind = Normal; name = State "C"; transitions = transC};
      { kind = Final; name = State "D"; transitions = transD}
    ]
  } in
  
  let sm = P4.Semantic.create_state_machine prog in
  let result = P4.Semantic.can_reach_final_state sm [] (State "A") in
  let expected = true in
  Alcotest.(check (bool))
    "Start state A reaches the final state D, exits infinite transition loop"
  expected
  result

let test_can_reach_final_state_startfinal_happy () =
  let open P4.Ast in
  let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [
      { kind = StartFinal; name = State "A"; transitions = []}]
  } in

  let sm = P4.Semantic.create_state_machine prog in
  let result = P4.Semantic.can_reach_final_state sm [] (State "A") in
  let expected = true in
  Alcotest.(check (bool))
    "StartFinal state A is reached"
  expected
  result

(* *********************************************************** *)
(*                          SAD TESTS                          *)
(* *********************************************************** *)

let test_can_reach_final_state_sad () =
  let open P4.Ast in
  let open Test_helper in
  let transA = 
      [
      (Transition (Event "e1", None, State "B", []));
      ] in
  let transB = 
      [
      (Transition (Event "e2", None, State "A", []))
      ] in
  let transC = 
      [] in
    
  let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [
      { kind = Start; name = State "A"; transitions = transA};
      { kind = Normal; name = State "B"; transitions = transB};
      { kind = Final; name = State "C"; transitions = transC}
    ]
  } in
  
  let sm = P4.Semantic.create_state_machine prog in
  let result = P4.Semantic.can_reach_final_state sm [] (State "A") in
  let expected = false in
  Alcotest.(check (bool))
    "Start state A never reaches the final state C"
  expected
  result

let test_can_reach_final_state_cycle_sad () =
  let open P4.Ast in
  let open Test_helper in
  let transA = 
      [
      (Transition (Event "e1", None, State "B", []))
      ] in
  let transB = 
      [
      (Transition (Event "e2", None, State "C", []))
      ] in
  let transC = 
      [
      (Transition (Event "e3", None, State "A", []))
      ] in
  let transD =
      [] in
    
  let prog = {
    machine_name = "M";
    variables = [];
    inputs = [];
    states = [
      { kind = Start; name = State "A"; transitions = transA};
      { kind = Normal; name = State "B"; transitions = transB};
      { kind = Normal; name = State "C"; transitions = transC};
      { kind = Final; name = State "D"; transitions = transD}
    ]
  } in
  
  let sm = P4.Semantic.create_state_machine prog in
  let result = P4.Semantic.can_reach_final_state sm [] (State "A") in
  let expected = false in
  Alcotest.(check (bool))
    "Start state A never reaches the final state C"
  expected
  result

(* *********************************************************** *)
(*                FUN CHECK_START_REACHES_FINAL                *)
(* *********************************************************** *)

(* *********************************************************** *)
(*                         HAPPY TEST                          *)
(* *********************************************************** *)

let test_check_start_reaches_final_happy () =
  let open P4.Ast in
  let open Test_helper in
  let transA = 
        [
        (Transition (Event "e1", None, State "B", []));
        (Transition (Event "e2", None, State "C", []))
        ] in
  let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Start; name = State "A"; transitions = transA };
        { kind = Final; name = State "B"; transitions = [] };
        { kind = Normal; name = State "C"; transitions = [] }
      ]
  } in
    let sm = P4.Semantic.create_state_machine prog in
    let result = P4.Semantic.check_start_reaches_final sm in
 
    Alcotest.(check (list warning_testable))
      "Returns empty warning list if start state reaches final state"
      []
      result
  
let test_check_start_reaches_startfinal_happy () =
  let open P4.Ast in
  let open Test_helper in
  let transA = 
        [] in
  let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = StartFinal; name = State "A"; transitions = transA };
        ]
  } in
    let sm = P4.Semantic.create_state_machine prog in
    let result = P4.Semantic.check_start_reaches_final sm in
 
    Alcotest.(check (list warning_testable))
      "Returns empty warning list if start state reaches final state"
      []
      result
  

(* *********************************************************** *)
(*                          SAD TEST                           *)
(* *********************************************************** *)

let test_check_start_reaches_final_sad () =
  let open P4.Ast in
  let open Test_helper in
  let transA = 
        [] in
  let prog = {
      machine_name = "M";
      variables = [];
      inputs = [];
      states = [
        { kind = Start; name = State "A"; transitions = transA };
        ]
  } in
    let sm = P4.Semantic.create_state_machine prog in
    let result = P4.Semantic.check_start_reaches_final sm in
 
    Alcotest.(check (list warning_testable))
      "Returns empty warning list if start state reaches final state"
      []
      result

(* *********************************************************** *)
(*                          RUN TESTS                          *)
(* *********************************************************** *)

let () =
  Alcotest.run "Semantic Tests" [
    "semantic", [
      Alcotest.test_case "collect_states"               `Quick test_collect_states_happy;
      Alcotest.test_case "get_start_states"             `Quick test_get_start_states_start_happy;
      Alcotest.test_case "get_start_states"             `Quick test_get_start_states_startfinal_happy;
      Alcotest.test_case "get_start_states"             `Quick test_get_start_states_multiple_start_states;
      Alcotest.test_case "get_start_states"             `Quick test_get_start_states_no_start_state;
      Alcotest.test_case "get_final_states"             `Quick test_get_final_states_happy;
      Alcotest.test_case "collect_transitions"          `Quick test_collect_transitions_happy;
      Alcotest.test_case "collect_g_variables"          `Quick test_collect_g_variables_happy;
      Alcotest.test_case "check_number_of_states"       `Quick test_check_number_of_states_happy;
      Alcotest.test_case "check_number_of_states"       `Quick test_check_number_of_states_sad;
      Alcotest.test_case "check_valid_transition"       `Quick test_check_valid_transition_happy;
      Alcotest.test_case "check_valid_transition"       `Quick test_check_valid_transition_sad;
      Alcotest.test_case "check_duplicate_state_names"  `Quick test_check_duplicate_state_names_happy;
      Alcotest.test_case "check_duplicate_state_names"  `Quick test_check_duplicate_state_names_sad;
      Alcotest.test_case "check_duplicate_transitions"  `Quick test_check_duplicate_transitions_happy;
      Alcotest.test_case "check_duplicate_transitions"  `Quick test_check_duplicate_transitions_sad;
      Alcotest.test_case "successors"                   `Quick test_successors_happy;
      Alcotest.test_case "successors"                   `Quick test_successors_sad;
      Alcotest.test_case "can_reach_final_state"        `Quick test_can_reach_final_state_happy;
      Alcotest.test_case "can_reach_final_state"        `Quick test_can_reach_final_state_cycle_happy;
      Alcotest.test_case "can_reach_final_state"        `Quick test_can_reach_final_state_startfinal_happy;
      Alcotest.test_case "can_reach_final_state"        `Quick test_can_reach_final_state_sad;
      Alcotest.test_case "can_reach_final_state"        `Quick test_can_reach_final_state_cycle_sad;
      Alcotest.test_case "check_start_reaches_final"    `Quick test_check_start_reaches_final_happy;
      Alcotest.test_case "check_start_reaches_final"    `Quick test_check_start_reaches_startfinal_happy;
      ];
]