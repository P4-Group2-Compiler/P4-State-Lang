open Semantic
open Ast

(*Do? Body?*)

(**************************************************************************************************************************************************)
let dotOpsBuf = Buffer.create 16 

let someOps lst =
    match lst with
    | [] -> false
    | _ -> true

let rec ops_to_stringList (ops : operation list) = 
  match ops with
  | [] -> []
  | h :: t -> op_to_string h :: ops_to_stringList t 

(*Strings of transitions*)
let stringify_trans (s1, e, expr_option, s2, ops) =
  let s1_str = state_to_string s1 in
  let e_str = event_to_string e in
  let eo_str = expr_option_to_string expr_option in
  let s2_str = state_to_string s2 in
  let ops_str_lst = ops_to_stringList ops in

    if someOps ops_str_lst then
       let rec addEachOps = function
        | [] -> ()
        | h :: t -> Buffer.add_string dotOpsBuf h; 
        addEachOps t 
      in
      addEachOps ops_str_lst;
      Printf.sprintf "%s -> %s [label=\"%s%s\"];\n" s1_str s2_str e_str eo_str
    else
      Printf.sprintf "%s -> %s [label=\"%s%s\"];\n" s1_str s2_str e_str eo_str
  
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
(*Strings of vars*)
let stringify_var v =
  let var_v = var_to_string v in
  Printf.sprintf "%s" var_v

let var_string_list (v : (var_decl) list) =
  List.map stringify_var v

(**************************************************************************************************************************************************)
(*Function that takes the entire statemachine*)
let graphFromStatemachine (sm : (statemachine)) =

  let smName = sm.statemachine_name in
  let varsList = var_string_list sm.g_variables in
  let startState = start_string sm.start_state in
  let finalStateList = final_string_list sm.final_state in
  let transList = trans_string_list sm.transitions in

  let dot_topSyntax = "digraph " ^ smName ^ " {\n rankdir = LR;\n graph [label = " ^ smName ^ "; labelloc = t;]\n node [shape = circle;];\n null [shape = point;];\n" in
  let dot_bottomSyntax = "}" in
  let dotBuf = Buffer.create 16 in
  
  (*Helper function for add strings to the DOT-buffer from the stringlists of transitions and final states*)
  let rec addEachString = function
  | [] -> ()
  | h :: t -> Buffer.add_string dotBuf h; addEachString t in

  Buffer.add_string dotBuf dot_topSyntax;
  Buffer.add_string dotBuf startState;
  addEachString transList;
  addEachString finalStateList;

(**************************************************************************************************************************************************)


(**************************************************************************************************************************************************)
(*Adds variables as a list, if there are any. Otherwise finishes the DOT-syntax.*)

  let someVars lst =
    match lst with
    | [] -> false
    | _ -> true
  in

  if someVars varsList then
    let dotBuf_Vars = Buffer.create 16 in
    let dot_varTopSyntax = "Variables [label = \"Variables: \n" in
    let dot_varBottomSyntax = "Variables -> null [style = invis;];\n" in

    let rec addEachVar = function
    | [] -> ()
    | h :: t -> Buffer.add_string dotBuf_Vars (h ^ "\n"); addEachVar t in

    Buffer.add_string dotBuf_Vars dot_varTopSyntax;
    addEachVar varsList;
    Buffer.add_string dotBuf_Vars "\"";
    Buffer.add_string dotBuf_Vars "\nshape = rectangle;];\n"; 
    Buffer.add_string dotBuf_Vars dot_varBottomSyntax;
    Buffer.add_buffer dotBuf dotBuf_Vars;
    Buffer.add_string dotBuf dot_bottomSyntax;
  else
    Buffer.add_string dotBuf dot_bottomSyntax;

(**************************************************************************************************************************************************)
  let oc = open_out "../output/DOT/graph.gv" in
  Buffer.output_buffer oc dotBuf;
  close_out oc;
  ignore (Sys.command "cd ../output/DOT && dot -Tpng graph.gv -o graph.png");
;