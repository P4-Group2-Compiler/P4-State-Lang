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

    let (sm, _warnings) = Semantic.analyse ast in
    Typechecker.type_program sm;
    
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

let expect_warning filename () =
  match parse_and_check_file filename with
  | Accepted ->
      ()
  | Rejected msg ->
      Alcotest.failf "Expected %s to be accepted with warning, but got: %s" filename msg      

let valid_tests =
  [
    Alcotest.test_case "AT-01 minimal valid state machine" `Quick
      (expect_accepted "acceptance/valid/at01_start_final_state.dsl");

    Alcotest.test_case "AT-02 valid machine with guard and variable" `Quick
      (expect_accepted "acceptance/valid/at02_guard_and_variable.dsl");

     Alcotest.test_case "AT-03 implicit transition behavior" `Quick
      (expect_accepted "acceptance/valid/at03_implicit_transition.dsl"); 

    Alcotest.test_case "AT-11 Same Event Transitions" `Quick
      (expect_accepted "acceptance/valid/at11_same_event_transistions.dsl");

    Alcotest.test_case "AT-12 Guard Variable railroaded" `Quick
      (expect_accepted "acceptance/valid/at12_guard_and_variable_railroaded.dsl");

    Alcotest.test_case "AT-13 Valid Binops" `Quick
      (expect_accepted "acceptance/valid/at13_binop.dsl");

    Alcotest.test_case "AT-14 Valid DO" `Quick
      (expect_accepted "acceptance/valid/at14_DO.dsl");

    Alcotest.test_case "AT-15 Valid AUTO" `Quick
      (expect_accepted "acceptance/valid/at15_AUTO.dsl");

    Alcotest.test_case "AT-16 Valid Input" `Quick
      (expect_accepted "acceptance/valid/at16_Input.dsl");
      
  ]

let invalid_tests =
  [
    Alcotest.test_case "AT-04 missing start state" `Quick
      (expect_rejected "acceptance/invalid/at04_missing_start.dsl");

    Alcotest.test_case "AT-05 missing statemachine block" `Quick
      (expect_rejected "acceptance/invalid/at05_missing_statemachine_block.dsl");

    Alcotest.test_case "AT-06 transition to undefined state" `Quick
      (expect_rejected "acceptance/invalid/at06_transition_to_undefined_state.dsl");
  ]

let warning_tests =
  [
     Alcotest.test_case "AT-07 duplicate transition" `Quick
      (expect_warning "acceptance/warning/at07_duplicate_transition.dsl"); 

    Alcotest.test_case "AT-08 unreachable final state" `Quick
      (expect_warning "acceptance/warning/at08_unreachable_final_state.dsl");

    Alcotest.test_case "AT-09 no final state" `Quick
      (expect_warning "acceptance/warning/at09_no_final_state.dsl");
      
    Alcotest.test_case "AT-10 empty states" `Quick
      (expect_warning "acceptance/warning/at10_empty_states.dsl");
      
    
  ]  

let () =
  Alcotest.run "Acceptance tests"
    [
      ("valid programs", valid_tests);
      ("invalid programs", invalid_tests);
      ("warning programs", warning_tests);
    ]
