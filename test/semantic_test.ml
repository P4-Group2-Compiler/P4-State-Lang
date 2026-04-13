open P4

let test_collect_states () = 
  (* ARRANGE *)
  let open P4.Ast in
  let state name =
    { kind = Normal; name = State name; transitions = [] }
  in
  let prog = {
    machine_name = "M";
    states = [ state "A"; state "B" ];
  } in

  (* ACT *)
  let result = P4.Semantic.collect_states prog in

  (*ASSERT*)
  let expected = [ State "A"; State "B" ] in
  Alcotest.(check (list string))
    "collect_states returns correct names"
    (List.map Semantic.state_to_string expected)
    (List.map Semantic.state_to_string result)

let () =
  Alcotest.run "Semantic Tests" [
    "semantic", [
      Alcotest.test_case "collect_states" `Quick test_collect_states;
    ];
  ]
