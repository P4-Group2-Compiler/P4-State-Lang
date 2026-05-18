open Ast
open Semantic

(* Naming states as reserved C-keywords (if, for, int, while, switch etc.) would cause problems
  if they are not prefixed with something like "STATE_" so that is done here: *)
let get_state_name_prefixed = function
  | State state_string -> "STATE_" ^ state_string

(* This one is then for getting the literal state name without a prefix.
  Only use it for actual strings inside the C code (like the strings in state_names[])*)
let get_state_name_literal = function
  | State state_string -> state_string

let get_event_name_string = function
  | Event event_string -> event_string
  | Auto -> "AUTO"

let rec convert_expr_to_string = function
  (* The base single "atoms" of an expression: *)
  | Ecst (Cint n) -> string_of_int n
  | Ecst (Cbool true)  -> "1"
  | Ecst (Cbool false) -> "0"
  | Ecst (Cstring string) -> string
  | Eident { id } -> "VAR_" ^ id

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
  | Var_decl (string, int) -> Printf.sprintf "int VAR_%s = %d" (string) (int)

let operation_to_c = function
  | Do (id, expr) ->
      Printf.sprintf "VAR_%s = %s;"
        (id.id)
        (convert_expr_to_string expr)


(*======================================================================================*)
let generate_c_code ir =

  let start_state_name = get_state_name_prefixed ir.start_state in

  let c_file = "../output/c/generated_state_machine.c" in
  let c_out_channel = open_out c_file in

  (* This is just a wrapper for the "fprint"-function: *)
  let emit_c fmt = Printf.fprintf c_out_channel fmt in
(*-------------------------------------------------------------------------------------*)
  (* Includes: *)
  emit_c "#include <stdio.h>\n";
  emit_c "#include <string.h>\n";
(*-------------------------------------------------------------------------------------*)
  (* Macros to color and format the terminals print-output (PURELY COSMETIC, no "functional" use) *)
  (* The codes are called "ANSI Escape Codes". More info here: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797 *)
  (* TEXT_RESET turns the text-format back to "default", otherwise all future text would be red after using RED for example *)
  emit_c "#define RED           \"\\033[31m\"\n";
  emit_c "#define ORANGE        \"\\033[38;5;208m\"\n";
  emit_c "#define YELLOW        \"\\033[93m\"\n";
  emit_c "#define GREEN         \"\\033[92m\"\n";
  emit_c "#define BOLD          \"\\033[1m\"\n";
  emit_c "#define UN_BOLD       \"\\033[22m\"\n";
  emit_c "#define TEXT_RESET    \"\\033[0m\"\n";
  emit_c "\n\n";
(*-------------------------------------------------------------------------------------*)
  emit_c "/*================================================================================================*/\n";  
  (* The State ENUM: *)
  emit_c "typedef enum {\n";
  List.iter (fun state -> 
    emit_c "    %s,\n" (get_state_name_prefixed state)
  ) ir.states;
  emit_c "} State;\n\n";
(*-------------------------------------------------------------------------------------*)
  (* We use this to track the current_state: *)
  emit_c "State global_current_state = %s;\n\n" start_state_name;
(*-------------------------------------------------------------------------------------*)
  (* This is an array to hold the state names (as strings) so they can be printed to the terminal: *)
  emit_c "const char* state_names[] = {";
  List.iter (fun state ->
    emit_c " \"%s\"," (get_state_name_literal state)
  ) ir.states;
  emit_c " };\n\n";
(*-------------------------------------------------------------------------------------*)
  (* Global variables: *)
  List.iter (fun var ->
    emit_c "%s;\n" (convert_var_to_string var) 
  ) ir.g_variables;

  emit_c "\n";
(*-------------------------------------------------------------------------------------*)
  emit_c "/*================================================================================================*/\n";
  (* Print helper functions primarily for making the C code more readable: *)

  (* Print GUARD BLOCK message: *)
  emit_c "void print_guard_block_msg(const char* transition, const char* guard) {\n";
  emit_c "    printf(ORANGE BOLD\"| Event \\\"%%s\\\" was blocked by the guard: %%s \\n\"TEXT_RESET, transition, guard);\n}\n";
  (* Print UNRECOGNIZED EVENT message: *)
  emit_c "void print_unrecognized_event_msg(const char* event) {\n";
  emit_c "    printf(RED BOLD\"| Event \\\"%%s\\\" is unrecognized in state (%%s)\\n\"TEXT_RESET, event, state_names[global_current_state]);\n}\n";
  (* Print the GREEN "[src]--->[dst]" message: *)
  emit_c "void pretty_print_transition(State src_state, State dst_state) {\n";
  emit_c "    printf(GREEN BOLD\"| (%%s) ---> (%%s)\\n\"TEXT_RESET, state_names[src_state], state_names[dst_state]);\n";
  emit_c "}\n";
  
(*-------------------------------------------------------------------------------------*)
  emit_c "/*================================================================================================*/\n";  
  (* The event_match() and transition_to() helper functions: *)

  (* MATCH EVENT strings: *)
  emit_c "int event_match(const char* input, const char* event) {\n";
  emit_c "    return (strcmp(input, event) == 0);\n}\n";

  emit_c "void transition_to(State dst_state) {\n";
  emit_c "    pretty_print_transition(global_current_state, /*--->*/ dst_state);\n";
  emit_c "    global_current_state = dst_state;\n";
  emit_c "}\n";

(*-------------------------------------------------------------------------------------*)
  emit_c "/*================================================================================================*/\n";
  (* The "state_machine_check-function" is now just here to avoid the extra state_machine-file include *)
  (* It contains the switch-core of the C output with states as cases containing transitions *)
  
  emit_c "void state_machine_check(const char* input_event) {\n\n";
  emit_c "    const char* fired_event = input_event;\n";
  emit_c "    int is_first_pass = 1;\n\n";
  emit_c "    auto_recheck:\n";
  emit_c "    if (is_first_pass != 1) {\n";
  emit_c "        fired_event = \"AUTO\";\n";
  emit_c "    }\n";
  emit_c "    is_first_pass = 0;\n\n";

  emit_c "    switch (global_current_state) {\n";

  List.iter (fun state ->
    emit_c "    case %s:\n" (get_state_name_prefixed state);
    List.iter (fun (src_state, event, guard_expr, dst_state, ops) ->
      if src_state = state then
        match event with
        | Auto ->
          begin
            match guard_expr with
            | None ->
              emit_c "        if (event_match(fired_event, \"AUTO\")) {\n";
              
              (* Emit transition operations *)
              List.iter (fun op ->
                emit_c "            %s\n"
                  (operation_to_c op)
              ) ops;

              emit_c "            /*THEN*/ transition_to(%s); goto auto_recheck; }\n"
              (get_state_name_prefixed dst_state);
            | Some expr ->
              emit_c "        if (event_match(fired_event, \"AUTO\")) { /*AND*/ if (%s) {\n"
              (convert_expr_to_string expr);

              (* Emit transition operations *)
              List.iter (fun op ->
                emit_c "            %s\n"
                  (operation_to_c op)
              ) ops;

              emit_c "            /*THEN*/ transition_to(%s); goto auto_recheck; }\n"
              (get_state_name_prefixed dst_state);
              emit_c "            else { print_guard_block_msg(fired_event, \"%s\"); } }\n"
                (convert_expr_to_string expr)
          end
        | Event _ ->
          begin
            match guard_expr with
            | None ->
              emit_c "        if (event_match(fired_event, \"%s\")) {\n"
                (get_event_name_string event);

              (* Emit transition operations *)
              List.iter (fun op ->
                emit_c "            %s\n"
                  (operation_to_c op)
              ) ops;

              emit_c "            /*THEN*/ transition_to(%s); goto auto_recheck; }\n"
              (get_state_name_prefixed dst_state);
            | Some expr ->
              emit_c "        if (event_match(fired_event, \"%s\")) { /*AND*/ if (%s) {\n"
                (get_event_name_string event)
                (convert_expr_to_string expr);

                (* Emit transition operations *)
                List.iter (fun op ->
                  emit_c "            %s\n"
                    (operation_to_c op)
                ) ops;

                emit_c "            /*THEN*/ transition_to(%s); goto auto_recheck; }\n"
                (get_state_name_prefixed dst_state);
              emit_c "            else { print_guard_block_msg(fired_event, \"%s\"); /*THEN*/ break; } }\n"
                (convert_expr_to_string expr)
          end
    ) ir.transitions;
    emit_c "\n";
    emit_c "        goto unrecognized_input_fallback;\n"
  ) ir.states;

  emit_c "    default:\n";
  emit_c "        unrecognized_input_fallback:\n";
  emit_c "        if (fired_event != \"AUTO\") {\n";
  emit_c "            print_unrecognized_event_msg(fired_event);\n";
  emit_c "        }\n";
  emit_c "        break;\n";
  emit_c "    }\n}\n\n";
(*-------------------------------------------------------------------------------------*)
  emit_c "/*================================================================================================*/\n";  
  (* main() + the main state machine loop that takes the users input: *)
  emit_c "int main(void) {\n";
  emit_c "    #define STATE_MACHINE_RUNNING 1\n";
  emit_c "    state_machine_check(\"AUTO\");\n";
  emit_c "    while (STATE_MACHINE_RUNNING) {\n";

  emit_c "        printf(YELLOW BOLD\"\\nCurrent state: \"TEXT_RESET\"(\"BOLD\"%%s\"UN_BOLD\")\\n\", state_names[global_current_state]);\n";
  List.iter (fun (Var_decl (name, _)) ->
    emit_c "        printf(\"%s = %%d\\n\", VAR_%s);\n"
      name
      name
  ) ir.g_variables;
  emit_c "        printf(YELLOW BOLD\"Event: \"TEXT_RESET);\n";

  emit_c "        char entered_event[256];\n";
  emit_c "        scanf(\"%%255s\", entered_event);\n";

  emit_c "        if (!event_match(entered_event, \"AUTO\")) {\n";
  emit_c "            state_machine_check(entered_event);\n";
  emit_c "        } else {\n";
  emit_c "            printf(RED BOLD\"| \\\"AUTO\\\" is a reserved event-keyword -- It is not allowed as manual input\\n\"TEXT_RESET);\n";
  emit_c "        }\n";
  emit_c "    }\n";
  emit_c "    return 0;\n";
  emit_c "}\n";
(*-------------------------------------------------------------------------------------*)
  close_out c_out_channel;

  (* Debug message to terminal, not for the .C file: *)
  Printf.printf "\n\nCreated C output file at: %s\n\n" c_file
