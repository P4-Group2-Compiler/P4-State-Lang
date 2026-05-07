open Semantic
open Ast

(**************************************************************************************************************************************************)
(*Strings of transitions*)

let stringify_trans (s1, e, _, s2, _ops) =
 let s1_str = state_to_string s1 in
 let e_str = event_to_string e in
 let s2_str = state_to_string s2 in
 Printf.sprintf "%s -> %s [label=\"%s\"];\n" s1_str s2_str e_str 

(*Outputs a list of DOT transition as strings, using above function*)
let trans_string_list (t : (state * event * _ * state * operation list) list) =
  List.map stringify_trans t

(*Prints string lists*)
let string_list_printer (t : (string) list) = 
  List.iter print_endline t

(**************************************************************************************************************************************************)
(*Strings of start states*)

let stringify_start s =
  let start_s = state_to_string s in
  Printf.sprintf "null -> %s;\n" start_s

let start_string (t : (state)) =
  stringify_start t

(**************************************************************************************************************************************************)
(*Strings of final states*)

let stringify_final s =
  let final_s = state_to_string s in
  Printf.sprintf "%s [shape = doublecircle;];\n" final_s
  
let final_string_list (t : (state) list) =
  List.map stringify_final t

(**************************************************************************************************************************************************)
(*Function that takes the entire statemachine*)

let graphFromStatemachine (sm : (statemachine)) =

  let smName = sm.statemachine_name in
  let startState = start_string sm.start_state in
  let finalStateList = final_string_list sm.final_state in
  let transList = trans_string_list sm.transitions in

  let dot_topSyntax = "digraph " ^ smName ^ " {\n rankdir = LR;\n node [shape = circle;];\n null [shape = point;];\n" in
  let dot_bottomSyntax = "}" in

  let dotBuf = Buffer.create 16 in
  
  (*Helper function for getting each string in the stringlists of transitions and final states*)
  let rec addEachString = function
  | [] -> ()
  | h :: t -> Buffer.add_string dotBuf h; addEachString t in

  Buffer.add_string dotBuf dot_topSyntax;
  Buffer.add_string dotBuf startState;
  addEachString transList;
  addEachString finalStateList;
  Buffer.add_string dotBuf dot_bottomSyntax;

  let oc = open_out "../output/DOT/graph.gv" in
  Buffer.output_buffer oc dotBuf;
  close_out oc;
  ignore (Sys.command "cd ../output/DOT && dot -Tpng graph.gv -o graph.png");
;