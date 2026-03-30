(*Some hardcoded dfa in DOT language. 
Please note, different commands for different types - can't mix and match strings and chars like DOT can.*)

print_endline "Hello world!";

(*
let () =
  let buf = Buffer.create 6000 in
  Buffer.add_string buf "digraph {";
  Buffer.add_string buf "node [shape = circle;];";
  Buffer.add_string buf "null [shape = point;];";
  Buffer.add_string buf " null -> q1;";
  Buffer.add_string buf "q1 -> q1;";
  Buffer.add_string buf "q1 -> q2;";
  Buffer.add_string buf "q2 [shape = doublecircle;]";
  Buffer.add_string buf "}";

  let oc = open_out "newfile.txt" in
  Buffer.output_buffer oc buf;
  close_out oc
;;

print_endline "I did it, in the test!";
*)