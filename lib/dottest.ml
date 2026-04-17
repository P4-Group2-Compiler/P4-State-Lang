open Semantic
open Ast

(**************************************************************************************************************************************************)
(*IN USE! Strings of transitions*)

(*Creates actual strings from transitions of type (state, event, state) in DOT transition format*)
let stringify_trans (s1, e, s2) =
 let s1_str = state_to_string s1 in
 let e_str = event_to_string e in
 let s2_str = state_to_string s2 in
 Printf.sprintf "%s -> %s [label=\"%s\"];" s1_str s2_str e_str 

(*Outputs a list of DOT transition as strings, using above function*)
let trans_string_list (t : (state * event * state) list) =
  List.map stringify_trans t

(*Prints string of the transitions*)
let string_list_printer (t : (string) list) = 
  List.iter print_endline t

(**************************************************************************************************************************************************)
(*IN USE! Printer, transitions as they are - not strings*)

(*Prints (state, event, state) in DOT syntax. NB! Does NOT return transitions as strings!*)
let transition_to_string (s1, e, s2) =    
  Printf.printf "%s -> %s [label=\"%s\";];"
  (state_to_string s1)
  (state_to_string s2)
  (event_to_string e)

(*Prints all transitions, as modified by transition_to_string. NB! Does NOT return transitions as strings!*)
let printer (t : (state * event * state) list) = 
  List.iter transition_to_string t

(**************************************************************************************************************************************************)
(*IN USE! String buffer*)

let dot_topSyntax = "digraph { rankdir = LR; node [shape = circle;]; null [shape = point;];"
let dot_bottomSyntax = "}"

(*Function for putting transitions into a buffer, with full syntax*)
let dot_syntax_buffer str1 str2 (strlist : (string) list) = 
  let dotBuf = Buffer.create 16 in
  Buffer.add_string dotBuf str1;

  (*Helper function for getting each string in the stringlist of transitions*)
  let rec addEachString = function
  | [] -> ()
  | h :: t -> Buffer.add_string dotBuf h; addEachString t in

  addEachString strlist;
  Buffer.add_string dotBuf str2;

  let oc = open_out "graph.gv" in
  Buffer.output_buffer oc dotBuf;
  close_out oc;
;