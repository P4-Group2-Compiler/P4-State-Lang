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

(*Prints string lists*)
let string_list_printer (t : (string) list) = 
  List.iter print_endline t

(**************************************************************************************************************************************************)
(*WIP* Strings of start states. 
NB! I fiddled with the semantics' statecmachine and get_start_states. 
Everything's a state list*)
let stringify_start s =
  let start_s = state_to_string s in
  Printf.sprintf "null -> %s;" start_s

let start_string_list (t : (state) list) =
  List.map stringify_start t

(**************************************************************************************************************************************************)
(*WIP* Strings of final states*)
let stringify_final s =
  let final_s = state_to_string s in
  Printf.sprintf "%s [shape = doublecircle;];" final_s
  
let final_string_list (t : (state) list) =
  List.map stringify_final t

(**************************************************************************************************************************************************)
(*IN USE! Printer, transitions as they are - not strings*)
(*Not immediately useful right now, but keep just in case*)

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
(*The idea is:
dot_topSyntax + transitions + dot_bottomSyntax = full dot syntax for the .gv file*)

(*Need to add the  [null transition] to the top.*)
(*Need to add [doublecircles to the final states in the bottom.*)
let dot_topSyntax = "digraph { rankdir = LR; node [shape = circle;]; null [shape = point;];"
let dot_bottomSyntax = "}"

(*Function for putting transitions into a buffer, with full syntax*)
let dot_syntax_buffer str1 str2 (strlist : (string) list) (strlist2 : (string) list) (strlist3 : (string) list) = 
  let dotBuf = Buffer.create 16 in

(*Helper function for getting each string in the stringlist of transitions*)
  let rec addEachString = function
  | [] -> ()
  | h :: t -> Buffer.add_string dotBuf h; addEachString t in

  (*For adding dot_topSyntax*)
  Buffer.add_string dotBuf str1;

  (*Using above function, adds all transitions*)
  addEachString strlist;

  (*Adds null pointer transition to start state(s)*)
  addEachString strlist2;

  (*Adds doublecircles to final states, final strings list*)
  addEachString strlist3;

  (*For adding dot_bottomSyntax*)
  Buffer.add_string dotBuf str2;

  let oc = open_out "graph.gv" in
  Buffer.output_buffer oc dotBuf;
  close_out oc;
;

(*TRASHPILE:

(*Stolen from .main, am not allowed to call it from the main module for some reason*)
let string_of_state_kind = function
  | Normal -> "Normal"
  | Start -> "Start"
  | Final -> "Final"

(*Creates strings of a state kind*)
let state_kind_to_string (state_kind) =
  let st_k = string_of_state_kind state_kind in
  Printf.sprintf "%s" st_k

(*Creates list of state kind*)
let state_kind_string_list (stk : (state_kind) list) =
  List.map state_kind_to_string stk
*)