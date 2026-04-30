open Ast
open Semantic

let state_string_identifier = function
  | Ast.State state_string -> state_string

let event_string_identifier = function
  | Ast.Event event_string -> event_string


let rec convert_expr_to_string = function
  | Ecst (Cint n) -> string_of_int n
  | Ecst (Cbool true)  -> "1"
  | Ecst (Cbool false) -> "0"
  | Ecst (Cstring string) -> string
  | Eident { id } -> id
  | Ebinop (Blt,  expr1, expr2) -> Printf.sprintf "(%s < %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)
  | Ebinop (Badd, expr1, expr2) -> Printf.sprintf "(%s + %s)" (convert_expr_to_string expr1) (convert_expr_to_string expr2)


(*======================================================================================*)
let generate_c_code ir =

  let start_state_name = state_string_identifier ir.start_state in

  let c_file = "output/c/generated_state_machine.c" in
  let c_out_channel = open_out c_file in
(*-------------------------------------------------------------------------------------*)
  (* Includes: *)
  Printf.fprintf c_out_channel "#include <stdio.h>\n";
  Printf.fprintf c_out_channel "#include <string.h>\n\n";
(*-------------------------------------------------------------------------------------*)
  (* The State ENUM: *)
  Printf.fprintf c_out_channel "typedef enum {\n    ";
  List.iter (fun state -> 
    Printf.fprintf c_out_channel "%s,\n    " (state_string_identifier state)
  ) ir.states;
  Printf.fprintf c_out_channel "} State;\n\n";
(*-------------------------------------------------------------------------------------*)
(* This is an array to hold the state names, so they can be printed to the terminal: *)
  Printf.fprintf c_out_channel "const char* state_names[] = {";
  List.iter (fun state ->
    Printf.fprintf c_out_channel "\"%s\", " (state_string_identifier state)
  ) ir.states;
  Printf.fprintf c_out_channel "};\n\n";
(*-------------------------------------------------------------------------------------*)
(* We use this to track the current_state: *)
Printf.fprintf c_out_channel "State global_current_state = %s;\n\n" start_state_name;
(*-------------------------------------------------------------------------------------*)
(* The "state_machine_step-function" is now just here to avoid the extra state_machine-file include *)
(* It contains the switch-core of the C output with states as cases containing transitions *)
Printf.fprintf c_out_channel "State state_machine_step(State current_state, const char* fired_event) {\n";
  Printf.fprintf c_out_channel "    switch (current_state) {\n";

  List.iter (fun state ->
    Printf.fprintf c_out_channel "    case %s:\n" (state_string_identifier state);
    List.iter (fun (src_state, event, guard_expr, dst_state) ->
      if src_state = state then
        match guard_expr with
        | None ->
        Printf.fprintf c_out_channel "        if (strcmp(fired_event, \"%s\") == 0) return %s;\n"
          (event_string_identifier event)
          (state_string_identifier dst_state)
        | Some expr ->
          Printf.fprintf c_out_channel "        if (strcmp(fired_event, \"%s\") == 0 && (%s)) return %s;\n"
          (event_string_identifier event)
          (convert_expr_to_string expr)
          (state_string_identifier dst_state)
    ) ir.transitions;
    Printf.fprintf c_out_channel "        return current_state;\n"
  ) ir.states;

  Printf.fprintf c_out_channel "    default:\nreturn current_state;\n";
  Printf.fprintf c_out_channel "    }\n}\n\n";
(*-------------------------------------------------------------------------------------*)
  (* main() + the main state machine loop that takes the users input: *)
  Printf.fprintf c_out_channel "#define STATE_MACHINE_RUNNING 1\n\n";
  Printf.fprintf c_out_channel "int main(void) {\n";
  Printf.fprintf c_out_channel "    while (STATE_MACHINE_RUNNING) {\n";
  Printf.fprintf c_out_channel "        printf(\"\\nCurrent state: %%s\\n\", state_names[global_current_state]);\n";
  Printf.fprintf c_out_channel "        printf(\"Enter event: \");\n";
  Printf.fprintf c_out_channel "        char entered_event[256];\n";
  Printf.fprintf c_out_channel "        scanf(\"%%255s\", entered_event);\n";
  Printf.fprintf c_out_channel "        global_current_state = state_machine_step(global_current_state, entered_event);\n";
  Printf.fprintf c_out_channel "    }\n";
  Printf.fprintf c_out_channel "    return 0;\n";
  Printf.fprintf c_out_channel "}\n";
(*-------------------------------------------------------------------------------------*)
  close_out c_out_channel;

  (* Debug message to terminal, not for the .C file: *)
  Printf.printf "\n\nCreated C output file at: %s\n\n" c_file