open Lexing
open Parsing
open P4
open Dotgen
open Ast
open Semantic

let string_of_state = function
  | State s -> s

let string_of_event = function
  | Event e -> e
  | Auto -> "AUTO"

let string_of_state_kind = function
  | Normal -> "Normal"
  | Start -> "Start"
  | Final -> "Final"
  | StartFinal -> "StartFinal"

let string_of_binop = function
  | Badd -> "+" | Bsub -> "-" | Bmul -> "*" | Bdiv -> "/" | Bmod -> "%"
  | Beq -> "==" | Bneq -> "!=" | Blt -> "<" | Ble -> "<=" | Bgt -> ">" | Bge -> ">="
  | Band -> "AND" | Bor -> "OR"

let string_of_constant = function
  | Cbool b -> string_of_bool b
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
  | Transition (Event event, None, target, _ops) ->
      Printf.printf "    ON %s GO %s\n"
        (event)
        (string_of_state target)
  | Transition (Event event, Some guard, target, _ops) ->
      Printf.printf "    ON %s IF %s GO %s\n"
        (event)
        (string_of_expr guard)
        (string_of_state target)
  | Transition (Auto, None, target, _ops) ->
      Printf.printf "    AUTO GO %s\n"
        (string_of_state target)
  | Transition (Auto, Some guard, target, _ops) ->
      Printf.printf "    AUTO IF %s GO %s\n"
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

(* Pattern matches the Warnings to print them in the correct form *)
let print_warning (statemachine, warning) =
  begin match warning with
  | DuplicateTransitions (src, event, dest) ->
      Printf.printf "WARNING {DuplicateTransition}:\n\tTransition: {FROM %s ON %s GO %s} is declared more than once\n"
        (string_of_state src)
        (string_of_event event)
        (string_of_state dest)
  | TooManyStates stateNum ->
      Printf.printf "WARNING {AmountOfStates}:\n\tLarge amount of states declared (%i > %i); output graph may become difficult to read\n"
        stateNum
        maxStates
  | UnreachableFinalState state ->
      Printf.printf "WARNING {UnreachableFinalState}:\n\tStart State: \"%s\" is unable to reach any Final State\n"
        (string_of_state state)
  | NoFinalState ->
      Printf.printf "WARNING {NoFinalState}:\n\tNo Final State has been declared - No input sequence will be accepted\n"
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
(*let ast =
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
in*)
let ast =
  try
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.Lexing_error msg ->
      Printf.printf "Lexing error: %s\n" msg;
      close_in chan;
      exit 1
| Parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    Printf.printf "Parse error at %s, line %d, position %d\n"
        pos.Lexing.pos_fname
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
    close_in chan;
    exit 1
in

(**************************************************************************************************)
  (*Collect functions in a functions, to use on the statemachine*)
  let statemachine, warnings =
    try Semantic.analyse ast with Semantic.Semantic_error msg ->
      Printf.printf "Semantic error: %s\n" msg;
      exit 1
  in
    Printf.printf "This is statemachine ---> %s <---!\n" statemachine.statemachine_name;
        
    (* Typecheck the program *)
        begin
        try
            Typechecker.type_program statemachine
        with Typechecker.Type_error msg ->
            Printf.printf "Type error: %s\n" msg;
            exit 1
        end;
    Codegen.generate_c_code statemachine;
    Dotgen.graphFromStatemachine statemachine;
  
  close_in chan;
  (* print_program ast; *)
  List.iter (fun warning -> print_warning (statemachine, warning)) warnings
  


  (* let transitions = Semantic.collect_transitions ast in
  Semantic.print_iter_trans transitions; *)