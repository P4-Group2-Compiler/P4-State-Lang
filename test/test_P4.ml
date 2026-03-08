let test_addition () =
  Alcotest.(check int) "2 + 2 equals 4" 4 (P4.Test_lexer.add 2 2)

let () =
  Alcotest.run "P4 Tests" [
    "math", [ Alcotest.test_case "addition" `Quick test_addition ]
  ]