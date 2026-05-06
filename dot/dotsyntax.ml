open P4
open Semantic
open Ast

(*NOT IN USE FOR NOW*)

let stringify_trans (s1, e, s2) = 
 let s1_str = state_to_string s1 in
 let e_str = event_to_string e in
 let s2_str = state_to_string s2 in
 Printf.sprintf "%s -> %s [label=\"%s\"];\n" s1_str s2_str e_str

let trans_string_list (t : (state * event * state) list) =
  List.map stringify_trans t

(*I need to make them a proper transition, a string that says "state -> state [labe="event"];"*)

(*****************************************************************************************************************************************)

(*dune exec ./dot/dotsyntax.exe*)
