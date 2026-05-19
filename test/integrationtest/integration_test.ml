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

let expect_gcc_valid filename () =
  match parse_and_check_file filename with
  | Rejected msg ->
    Alcotest.failf "Pipeline rejected %s: %s" filename msg
    | Accepted ->
    let gcc_result = Sys.command "gcc -fsyntax-only output/c/generated_state_machine.c 2>/dev/null" in
    Alcotest.(check int) "GCC accepts emitted C" 0 gcc_result

let valid_tests =
  [
    Alcotest.test_case "AT-01 minimal valid state machine" `Quick
      (expect_gcc_valid "valid_gcc.sm");

    Alcotest.test_case "AT-02 valid machine with guard and variable" `Quick
      (expect_gcc_valid "valid_gcc.sm");
  ]

let () =
  Alcotest.run "Integration tests"
    [
      ("integration_gcc_happy", valid_tests);
    ]
