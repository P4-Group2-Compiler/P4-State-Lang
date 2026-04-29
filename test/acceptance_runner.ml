open P4

type actual_result =
  | Accepted
  | Rejected of string

let parse_and_check_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = Parser.prog Lexer.token lexbuf in
    close_in chan;

    (* Run later pipeline stages here *)
    Typechecker.type_program ast;
    ignore (Semantic.collect_transitions ast);
    ignore (Semantic.analyse ast);

    Accepted
  with
  | Lexer.Lexing_error msg ->
      close_in_noerr chan;
      Rejected ("Lexing error: " ^ msg)

  | Parser.Error ->
      close_in_noerr chan;
      Rejected "Parse error"

  | Typechecker.Type_error msg ->
      close_in_noerr chan;
      Rejected ("Type error: " ^ msg)

  | exn ->
      close_in_noerr chan;
      Rejected (Printexc.to_string exn)

let expect_accepted filename () =
  match parse_and_check_file filename with
  | Accepted -> ()
  | Rejected msg ->
      Alcotest.failf "Expected %s to be accepted, but got: %s" filename msg

let expect_rejected filename () =
  match parse_and_check_file filename with
  | Rejected _ -> ()
  | Accepted ->
      Alcotest.failf "Expected %s to be rejected, but it was accepted" filename

let valid_tests =
  [
    Alcotest.test_case "AT-01 minimal valid state machine" `Quick
      (expect_accepted "acceptance/valid/at01_minimal_valid_state_machine.dsl");

    Alcotest.test_case "AT-02 valid machine with guard and variable" `Quick
      (expect_accepted "acceptance/valid/at02_valid_machine_with_guard_and_variable.dsl");

    Alcotest.test_case "AT-03 implicit transition behavior" `Quick
      (expect_accepted "acceptance/valid/at03_implicit_transition_behavior.dsl");
  ]

let invalid_tests =
  [
    Alcotest.test_case "AT-04 missing start state" `Quick
      (expect_rejected "acceptance/invalid/at04_missing_start_state.dsl");

    Alcotest.test_case "AT-05 missing statemachine block" `Quick
      (expect_rejected "acceptance/invalid/at05_missing_statemachine_block.dsl");

    Alcotest.test_case "AT-06 transition to undefined state" `Quick
      (expect_rejected "acceptance/invalid/at06_transition_to_undefined_state.dsl");
  ]

let () =
  Alcotest.run "Acceptance tests"
    [
      ("valid programs", valid_tests);
      ("invalid programs", invalid_tests);
    ]
    