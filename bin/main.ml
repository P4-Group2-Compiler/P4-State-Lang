open Lexing
open Parsing
open P4
open Dottest
open Ast
open Semantic


let string_of_state = function
  | State s -> s

let string_of_event = function
  | Event e -> e

let string_of_state_kind = function
  | Normal -> "Normal"
  | Start -> "Start"
  | Final -> "Final"

let string_of_binop = function
  | Badd -> "+" (*| Bsub -> "-" | Bmul -> "*" | Bdiv -> "/" | Bmod -> "%"
  | Beq -> "==" | Bneq -> "!=" *)| Blt -> "<" (*| Ble -> "<=" | Bgt -> ">" | Bge -> ">="
  | Band -> "and" | Bor -> "or"*)

let string_of_constant = function
  | Cbool b -> string_of_bool b
  | Cstring s -> s
  | Cint i -> string_of_int i

let rec string_of_expr = function
  | Ecst c -> string_of_constant c
  | Eident id -> id.id
  | Ebinop (op, e1, e2) ->
      Printf.sprintf "(%s %s %s)"
        (string_of_expr e1)
        (string_of_binop op)
        (string_of_expr e2)

let string_of_guard = function
  | Some expr -> string_of_expr expr
  | None -> ""

let print_transition = function
  | Transition (event, None, target) ->
      Printf.printf "    ON %s GO %s\n"
        (string_of_event event)
        (string_of_state target)
  | Transition (event, Some guard, target) ->
      Printf.printf "    ON %s IF %s GO %s\n"
        (string_of_event event)
        (string_of_expr guard)
        (string_of_state target)

let print_state st =
  Printf.printf "   State Type: %s State: %s\n" 
  (string_of_state_kind st.kind) 
  (string_of_state st.name);
  List.iter print_transition st.transitions

let print_program p =
  Printf.printf "Machine: %s\n" p.machine_name;
  List.iter print_state p.states

(***)

(***)

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.printf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in

  let ast =
    try
      Parser.prog Lexer.token lexbuf
    with
    | _ ->
        Printf.printf "Parse error\n";
        close_in chan;
        exit 1
  in
  (* Typecheck the program *)
begin
  try
    Typechecker.type_program ast
  with Typechecker.Type_error msg ->
    Printf.printf "Type error: %s\n" msg;
    exit 1
end;    

  let transition = Semantic.collect_transitions ast in  (*Printing out what will be transitions in DOT*)

    Dottest.printer transition;

  close_in chan;
  print_program ast;

  (* Debuggin print statement - TODO remove later *)
let check_start_state = Semantic.get_start_sates ast in
  match check_start_state with
  | _ -> Printf.printf "This code is running"

(* let transitions = Semantic.collect_transitions ast in
  Semantic.print_iter_trans transitions; *)

