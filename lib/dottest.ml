open Semantic
open Ast

let transition_to_string (s1, e, s2) =       (*Prints (state, event, state) in DOT syntax as string*)
  Printf.printf "%s -> %s [label=\"%s\"];"
  (state_to_string s1)
  (state_to_string s2)
  (event_to_string e)

let printer (t : (state * event * state) list) = (*Prints all transitions, as modified by transition_to_string*)
  List.iter transition_to_string t
  
(**************************************************************************************************************************************************)
  
let rec stringify f = function (*Not in use for now*)
| [] -> []
| h :: t -> f h :: stringify f t

(* let transList lst = stringify (transition_to_string) lst (*Not in use for now*)
let result = Semantic.transList (*Not in use for now. Just storing the list of collected transitions here, for now.*)
let stuff = Semantic.collect_transitions (*Not in use for now. Same as above*) *)
