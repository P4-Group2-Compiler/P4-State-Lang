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

let print_transition = function
  | Transition (event, target) ->
      Printf.printf "    ON %s GO %s\n"
        (string_of_event event)
        (string_of_state target)

let print_state st =
  Printf.printf "   State Type: %s State: %s\n" 
  (string_of_state_kind st.kind) 
  (string_of_state st.name);
  List.iter print_transition st.transitions

let print_program p =
  Printf.printf "Machine: %s\n" p.machine_name;
  List.iter print_state p.states

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

  (*************************************************************************************************)
  (*For checking DOT*)

  (*Collects transitions, start and final from the statemachine*)
  let transition = Semantic.collect_transitions ast in
  let starts = Semantic.get_start_states ast in
  let finals = Semantic.get_final_states ast in

  (*Stores transitions, start and final states as strings*)
  let storedTransStrings = Dottest.trans_string_list transition in
  let storedStartStrings = Dottest.start_string_list starts in
  let storedFinalStrings = Dottest.final_string_list finals in

  (*GRAPHVIZ FILE*)
  (*Buffs state machine into DOT syntax and creates .gv file*)
  Dottest.dot_syntax_buffer Dottest.dot_topSyntax Dottest.dot_bottomSyntax storedStartStrings storedFinalStrings storedTransStrings;

  (**************************************************************************************************)
  (*Collect functions in a functions, to use on the statemachine*)
  let statemachine = Semantic.analyse ast in
    Printf.printf "This is statemachine ---> %s <---!\n" statemachine.statemachine_name;
  
  close_in chan;
  (*print_program ast;*)

  (*dune exec ./main.exe -- test.sm*)

  (*
  (* Debuggin print statement - TODO remove later *)
  let check_start_state = Semantic.get_start_sates ast in
  match check_start_state with
  | _ -> Printf.printf "This code is running"
  *)

  (* let transitions = Semantic.collect_transitions ast in
  Semantic.print_iter_trans transitions; *)