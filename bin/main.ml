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
  | Badd -> "+" | Bsub -> "-" | Bmul -> "*" | Bdiv -> "/" | Bmod -> "%"
  | Beq -> "==" | Bneq -> "!=" | Blt -> "<" | Ble -> "<=" | Bgt -> ">" | Bge -> ">="
  | Band -> "AND" | Bor -> "OR"

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

let string_of_var = function
  | Var_decl (name, value) -> Printf.sprintf "%s = %d" name value

let print_vars vars =
  Printf.printf "Variables:\n";
  List.iter (fun v -> Printf.printf "  %s\n" (string_of_var v)) vars

let print_program p =
  Printf.printf "Machine: %s\n" p.machine_name;
  print_vars p.variables;
  List.iter print_state p.states

let print_warning (statemachine, warning) =
  begin match warning with
  | DuplicateTransitions (src, event, dest) ->
      Printf.printf "WARNING: Transition: {FROM %s ON %s GO %s} is declared more than once\n"
        (string_of_state src)
        (string_of_event event)
        (string_of_state dest)
  | TooManyStates stateNum ->
      Printf.printf "WARNING: Too many states declared (%i > %i); output graph may become difficult to read\n"
        stateNum
        maxStates
  end

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
    (* Loads the filename from CLI argument (Sys.argv) into the lexbuf position record *)
    lexbuf.lex_curr_p <- { 
    lexbuf.lex_curr_p with
    pos_fname = filename
    };
  
  (* 'Run' the compiler *)
let ast =
  try
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.Lexing_error msg ->
      Printf.printf "Lexing error: %s\n" msg;
      close_in chan;
      exit 1
  | Parser.Error ->
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

  (**************************************************************************************************)
  (*Collect functions in a functions, to use on the statemachine*)
  let statemachine, warnings = Semantic.analyse ast in
    Printf.printf "This is statemachine ---> %s <---!\n" statemachine.statemachine_name;
    Codegen.generate_c_code statemachine;
    Dottest.graphFromStatemachine statemachine;
  
  close_in chan;
  (* print_program ast; *)
  List.iter (fun warning -> print_warning (statemachine, warning)) warnings
  


  (* let transitions = Semantic.collect_transitions ast in
  Semantic.print_iter_trans transitions; *)