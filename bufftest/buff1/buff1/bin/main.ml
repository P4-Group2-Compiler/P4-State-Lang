(*Some hardcoded dfa in DOT language. 
Please note, different commands for different types - can't mix and match strings and chars like DOT can.*)

let () =
  let buf = Buffer.create 6000 in
  Buffer.add_string buf "digraph {";

  (*Adding the DOT direction, rankdir = "LR";*)
  Buffer.add_string buf "rankdir = \"LR\";";
  
  (*Adding the nodes and transitions, starting with the start "arrow"*)
  Buffer.add_string buf "node [shape = circle;];";
  Buffer.add_string buf "null [shape = point;];";
  Buffer.add_string buf "null -> q1;";

  (*State q1*)
  Buffer.add_string buf "q1 -> q1 [label = \"0\";]";
  Buffer.add_string buf "q1 -> q2 [label = \"1\";]";

  (*State q2*)
  Buffer.add_string buf "q2 -> q2 [label = \"1\";]";
  Buffer.add_string buf "q2 -> q1 [label = \"0\";]";
 
  Buffer.add_string buf "q2 [shape = doublecircle;]";
  Buffer.add_string buf "}";

  let oc = open_out "newdfa.gv" in
  Buffer.output_buffer oc buf;
  close_out oc;
;
print_endline "I did it!";

(*dot -Tpng somedfa.gv -o somedfa.png && sxiv somedfa.png*)

(*
digraph {
    rankdir = "LR";
    node [shape = circle;];
    null [shape = point;];
    
    null -> q1;
    
    q1 -> q1 [label = "0";];
    q1 -> q2 [label = "1";];
    q2 -> q2 [label = "1";];
    q2 -> q1 [label = "0";];
    
    q2 [shape = doublecircle;];
}
*)