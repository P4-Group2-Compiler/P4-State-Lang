(*Some hardcoded dfa in DOT language. Please note, different commands for different types - can't mix and match strings and chars like DOT can.*)
(*Add the "" in the DOT code via insertion in the buffer? Is it even needed? We will compile into pure DOT*)
(*
Tentative convention:
When we output a DOT-string, make sure to write ";" at the end of edges, and after the individual attributes of the edges.
ex. q0 -> q0 [label = "b,c"; len = 1.5;];
DOT will not accept spacing or sepearation between chars with ,; etc. without being encapsulated in "", which the Buffer in OCaml doesn't accept unless there's \ making it a string literal.

DOT does NOT accept: 
a , b
a,b
a b
*)
let dot_lang = "digraph {rankdir = \"LR\";node [shape = circle;];null [shape = point;];null -> q1;q1 -> q1 [label = \"0\";]q1 -> q2 [label = \"1\";]q2 -> q2 [label = \"1\";]q2 -> q1 [label = \"0\";]q2 [shape = doublecircle;]}"
let copy = dot_lang

let first_half = "digraph {rankdir = \"LR\";node [shape = circle;];null [shape = point;];null -> q1;q1 -> q1 [label = \"0\";]"
let second_half = "q1 -> q2 [label = \"1\";]q2 -> q2 [label = \"1\";]q2 -> q1 [label = \"0\";]q2 [shape = doublecircle;]}"

let () =
  let buf = Buffer.create 16 in
  let buf_first = Buffer.create 16 in
  let buf_second = Buffer.create 16 in
  let buf2 = Buffer.create 16 in

  Buffer.add_string buf_first first_half;
  Buffer.add_string buf_second second_half;

  Buffer.add_buffer buf buf_first;
  Buffer.add_buffer buf buf_second;
  (*Buffer.add_string buf "\nThis is a string of two buffers.";*)

  Buffer.add_string buf2 first_half;
  Buffer.add_string buf2 second_half;
  (*Buffer.add_string buf2 "\nThis is a string of two seperate strings.";*)

  let s = Buffer.sub buf 0 20 in
  let s = s ^ "\nThis is a substring." in 

  let oc = open_out "test.gv" in
  Buffer.output_buffer oc buf;

  let oc = open_out "test2.gv" in
  Buffer.output_buffer oc buf2;
  close_out oc;
;
print_endline s;

(*dot -Tpng somedfa.gv -o somedfa.png && sxiv somedfa.png*)