open P4

type actual_result =
  | Accepted
  | Rejected of string

let parse_and_check_file filename =
  Printf.printf "Working dir: %s\n" (Sys.getcwd ());
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = Parser.prog Lexer.token lexbuf in
    close_in chan;

    let (sm, _warnings) = Semantic.analyse ast in
    Typechecker.type_program sm;

    Codegen.generate_c_code sm;
    
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

let expect_gcc_happy filename () =
  match parse_and_check_file filename with
  | Rejected msg ->
    Alcotest.failf "Pipeline rejected %s: %s" filename msg
  | Accepted ->
    let gcc_result = Sys.command "gcc -fsyntax-only output/c/generated_state_machine.c 2>/dev/null" in
    Alcotest.(check int) "GCC accepts emitted C" 0 gcc_result

let expect_gcc_rejected filename () =
  match parse_and_check_file filename with
  | Accepted ->
      Alcotest.failf "Expected %s to be rejected, but pipeline accepted it" filename
  | Rejected _ ->
      ()

let happy_tests =
  [
    Alcotest.test_case "Feature complete statemachine" `Quick
      (expect_gcc_happy "happytest/feature_complete_gcc_happy.sm");
    Alcotest.test_case "Simple StartFinal statemachine" `Quick
      (expect_gcc_happy "happytest/simple_startfinal_happy.sm");
    Alcotest.test_case "Simple variable declaration statemachine" `Quick
      (expect_gcc_happy "happytest/simple_variable_happy.sm");
    Alcotest.test_case "Simple input declaration statemachine" `Quick
      (expect_gcc_happy "happytest/simple_input_happy.sm");
    Alcotest.test_case "Simple IF statemachine" `Quick
      (expect_gcc_happy "happytest/simple_IF_happy.sm");
    Alcotest.test_case "Simple IF statemachine" `Quick
      (expect_gcc_happy "happytest/simple_DO_happy.sm");
  ]

let sad_tests =
  [
    Alcotest.test_case "Duplicate variable assigned" `Quick
      (expect_gcc_rejected "sadtest/invalid_duplicate_variable_declaration.sm")
  ]

let () =
  Alcotest.run "Integration tests"
    [
      ("integration_gcc_happy", happy_tests);
      ("integration_gcc_sad", sad_tests)
    ]
