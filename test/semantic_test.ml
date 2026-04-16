open P4
(********************* FUN COLLECT_STATES *********************)
(********************* Happy test *********************)
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

let test_collect_transitions_happy () = assert true

(********************* Sad test *********************)

(****************************************************)


(* Initializing the tests. The de facto 'run' function *)
let () =
  Alcotest.run "Semantic Tests" [
    "semantic", [
      Alcotest.test_case "collect_states" `Quick test_collect_states_happy;
      Alcotest.test_case "collect_transitions" `Quick test_collect_transitions_happy;
    ];
  ]
