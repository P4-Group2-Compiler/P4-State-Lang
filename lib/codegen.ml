open Ast
open Semantic

(* ...to-string helper functions: *)
let get_state_name_string = function
  | State state_string -> state_string

let get_event_name_string = function
  | Event event_string -> event_string

let rec convert_expr_to_string = function
  (* The base single "atoms" of an expression: *)
  | Ecst (Cint n) -> string_of_int n
  | Ecst (Cbool true)  -> "1"
  | Ecst (Cbool false) -> "0"
  | Eident { id } -> id

  (* And the groups of different binop composite expressions that string-convert-recurses down to their atoms: *)
  (*[ +, -, *, /, % ]*)
  | Ebinop (Badd, expr1, expr2) -> Printf.sprintf "(%s + %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  | Ebinop (Bsub, expr1, expr2) -> Printf.sprintf "(%s - %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  | Ebinop (Bmul, expr1, expr2) -> Printf.sprintf "(%s * %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  | Ebinop (Bdiv, expr1, expr2) -> Printf.sprintf "(%s / %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  | Ebinop (Bmod, expr1, expr2) -> Printf.sprintf "(%s %% %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)

  (*[ ==, != ]*)
  | Ebinop (Beq,  expr1, expr2) -> Printf.sprintf "(%s == %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  | Ebinop (Bneq,  expr1, expr2) -> Printf.sprintf "(%s != %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)

  (*[ <, <=, >, >= ]*)
  | Ebinop (Blt,  expr1, expr2) -> Printf.sprintf "(%s < %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  | Ebinop (Ble,  expr1, expr2) -> Printf.sprintf "(%s <= %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  | Ebinop (Bgt,  expr1, expr2) -> Printf.sprintf "(%s > %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  | Ebinop (Bge,  expr1, expr2) -> Printf.sprintf "(%s >= %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  
  (*[ &&, || ]*)
  | Ebinop (Band,  expr1, expr2) -> Printf.sprintf "(%s && %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  | Ebinop (Bor,  expr1, expr2) -> Printf.sprintf "(%s || %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)

  (* | _ -> "NOT MATCHED" *)
(*--------------------------------------------------------------------------------------*)
let rec convert_var_to_string = function
  | Var_decl (string, int) -> Printf.sprintf "int %s = %d" (string) (int)

let operation_to_c = function
  | Do (id, expr) ->
      Printf.sprintf "%s = %s;"
        id.id
        (convert_expr_to_string expr)


(*======================================================================================*)
let generate_c_code ir =

  let start_state_name = get_state_name_string ir.start_state in

  let c_file = "../output/c/generated_state_machine.c" in
  let c_out_channel = open_out c_file in

  (* This is just a wrapper for the "fprint"-function: *)
  let emit_c fmt = Printf.fprintf c_out_channel fmt in
(*-------------------------------------------------------------------------------------*)
  (* Includes: *)
  emit_c "#include <stdio.h>\n";
  emit_c "#include <string.h>\n\n";
(*-------------------------------------------------------------------------------------*)
  (* Macros to color and format the terminals print-output (PURELY COSMETIC, no "functional" use) *)
  (* The codes are called "ANSI Escape Codes". More info here: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797 *)
  (* TEXT_RESET turns the text-format back to "default", otherwise all future text would be red after using RED for example *)
  emit_c "#define RED           \"\\033[31m\"\n";
  emit_c "#define YELLOW        \"\\033[93m\"\n";
  emit_c "#define YELLOW_BOLD   \"\\033[1;93m\"\n";
  emit_c "#define GREEN         \"\\033[1;32m\"\n";
  emit_c "#define BOLD          \"\\033[1m\"\n";
  emit_c "#define TEXT_RESET    \"\\033[0m\"\n\n";
(*-------------------------------------------------------------------------------------*)
  (* The State ENUM: *)
  emit_c "typedef enum {\n";
  List.iter (fun state -> 
    emit_c "    %s,\n" (get_state_name_string state)
  ) ir.states;
  emit_c "} State;\n\n";
(*-------------------------------------------------------------------------------------*)
(* This is an array to hold the state names (as strings) so they can be printed to the terminal: *)
  emit_c "const char* state_names[] = {";
  List.iter (fun state ->
    emit_c " \"%s\"," (get_state_name_string state)
  ) ir.states;
  emit_c " };\n\n";
(*-------------------------------------------------------------------------------------*)
  (* Helper functions primarily for making the C code more readable: *)

  (* Print GUARD BLOCK message: *)
  emit_c "void print_guard_block_msg(const char* transition, const char* guard) {\n";
  emit_c "    printf(RED\"\\\"%%s\\\" transition was \"BOLD\"blocked by guard:\"TEXT_RESET\" %%s \\n\"TEXT_RESET, transition, guard);\n}\n";
  (* Print UNRECOGNIZED EVENT message: *)
  emit_c "void print_unrecognized_event_msg(const char* event) {\n";
  emit_c "    printf(RED\"\\\"%%s\\\" is an \"BOLD\"unrecognized event \"TEXT_RESET RED\"for this state!\\n\"TEXT_RESET, event);\n}\n";
  (* Print the GREEN "[src]--->[dst]" message: *)
  emit_c "void pretty_print_transition(State src_state, State dst_state) {\n";
  emit_c "    printf(GREEN\"[%%s] ---> [%%s]\\n\"TEXT_RESET, state_names[src_state], state_names[dst_state]);\n}\n";
  (* MATCH EVENT strings: *)
  emit_c "int event_match(const char* input, const char* event) {\n";
  emit_c "    return (strcmp(input, event) == 0);\n}\n";

  emit_c "\n";
(*-------------------------------------------------------------------------------------*)
  (* We use this to track the current_state: *)
  emit_c "State global_current_state = %s;\n\n" start_state_name;
(*-------------------------------------------------------------------------------------*)
  (* Global variables: *)
  List.iter (fun var ->
    emit_c "%s;\n" (convert_var_to_string var) 
  ) ir.g_variables;

  emit_c "\n";
(*-------------------------------------------------------------------------------------*)
  (* The "state_machine_step-function" is now just here to avoid the extra state_machine-file include *)
  (* It contains the switch-core of the C output with states as cases containing transitions *)
  emit_c "State state_machine_step(State current_state, const char* fired_event) {\n";
  emit_c "    switch (current_state) {\n";

  List.iter (fun state ->
    emit_c "    case %s:\n" (get_state_name_string state);
    List.iter (fun (src_state, event, guard_expr, dst_state, ops) ->
      if src_state = state then
        match guard_expr with
        | None ->
          emit_c "        if (event_match(fired_event, \"%s\")) {\n"
            (get_event_name_string event);

          (* Emit transition operations *)
          List.iter (fun op ->
            emit_c "            %s\n"
              (operation_to_c op)
          ) ops;

          emit_c "            return %s;\n"
            (get_state_name_string dst_state);

          emit_c "        }\n";
        | Some expr ->
          emit_c "        if (event_match(fired_event, \"%s\")) {\n"
            (get_event_name_string event);

          emit_c "            if (%s) {\n"
            (convert_expr_to_string expr);

          (* Emit transition operations *)
          List.iter (fun op ->
            emit_c "                %s\n"
              (operation_to_c op)
          ) ops;

          emit_c "                return %s;\n"
            (get_state_name_string dst_state);

          emit_c "            }\n";

          emit_c "        }\n";
    ) ir.transitions;
    (*emit_c "        print_unrecognized_event_msg(fired_event);\n";
    emit_c "        return current_state;\n";*)
    emit_c "        goto unrecognized_input_fallback;\n"
  ) ir.states;

  emit_c "    default:\n";
  emit_c "    unrecognized_input_fallback:\n";
  emit_c "        print_unrecognized_event_msg(fired_event);\n";
  emit_c "        return current_state;\n";
  emit_c "    }\n}\n\n";
(*-------------------------------------------------------------------------------------*)
  (* main() + the main state machine loop that takes the users input: *)
  emit_c "int main(void) {\n";
  emit_c "    #define STATE_MACHINE_RUNNING 1\n";
  emit_c "    while (STATE_MACHINE_RUNNING) {\n";
  emit_c "        State initial_state = global_current_state;\n";
  emit_c "        printf(YELLOW_BOLD\"Current state: \"TEXT_RESET\"%%s\\n\", state_names[global_current_state]);\n";
  List.iter (fun (Var_decl (name, _)) ->
    emit_c "        printf(\"%s = %%d\\n\", %s);\n"
      name
      name
  ) ir.g_variables;
  emit_c "        printf(YELLOW\"Event: \"TEXT_RESET);\n";
  emit_c "        char entered_event[256];\n";
  emit_c "        scanf(\"%%255s\", entered_event);\n";
  emit_c "        global_current_state = state_machine_step(global_current_state, entered_event);\n";
  (*
  emit_c "        pretty_print_transition(initial_state, /*--->*/ global_current_state);\n";
  *)
  emit_c "        if (initial_state != global_current_state) {\n";
  emit_c "            pretty_print_transition(initial_state, /*--->*/ global_current_state);\n";
  emit_c "        }\n";
  emit_c "    }\n";
  emit_c "    return 0;\n";
  emit_c "}\n";
(*-------------------------------------------------------------------------------------*)
  close_out c_out_channel;

  (* Debug message to terminal, not for the .C file: *)
  Printf.printf "\n\nCreated C output file at: %s\n\n" c_file
