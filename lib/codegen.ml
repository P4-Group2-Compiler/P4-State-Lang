open Ast
open Semantic

let state_string_identifier = function
  | Ast.State state_string -> state_string

let event_string_identifier = function
  | Ast.Event event_string -> event_string

(*======================================================================================*)
let generate_c_code ir =

  let start_state_name = state_string_identifier ir.start_state in

  let c_file = "output/c/generated_state_machine.c" in
  let c_out_channel = open_out c_file in
(*-------------------------------------------------------------------------------------*)
  (* Includes: *)
  Printf.fprintf c_out_channel "#include \"state_machine.h\"\n";
  Printf.fprintf c_out_channel "#include <stdio.h>\n\n";
(*-------------------------------------------------------------------------------------*)
  (* The StateID ENUM: *)
  Printf.fprintf c_out_channel "typedef enum {\n    ";
  List.iter (fun state -> 
    Printf.fprintf c_out_channel "%s,\n    " (state_string_identifier state)
  ) ir.states;
  Printf.fprintf c_out_channel "\n    STATE_COUNT\n} StateID;\n\n";
(*-------------------------------------------------------------------------------------*)
  (* main() + StateMachine struct init + loop that populates the states[] array: *)
  Printf.fprintf c_out_channel "int main(void) {\n";
  Printf.fprintf c_out_channel "    StateMachine %s = {0};\n" ir.statemachine_name;
  List.iter (fun state ->
    let current_state_name = state_string_identifier state in
    Printf.fprintf c_out_channel "    %s.states[%s] = (State){.name = \"%s\"};\n"
      ir.statemachine_name current_state_name current_state_name
  ) ir.states;
  Printf.fprintf c_out_channel "    %s.state_count = STATE_COUNT;\n" ir.statemachine_name;

  Printf.fprintf c_out_channel "\n";
(*-------------------------------------------------------------------------------------*)
  (* .start_state + .current_state + .final_states[]: *)
  Printf.fprintf c_out_channel "    %s.start_state = %s.states[%s];\n" ir.statemachine_name ir.statemachine_name start_state_name;
  Printf.fprintf c_out_channel "    %s.current_state = %s.start_state;\n" ir.statemachine_name ir.statemachine_name;
  List.iteri (fun i state ->
    let current_state_name = state_string_identifier state in
    Printf.fprintf c_out_channel "    %s.final_states[%d] = %s.states[%s];\n"
      ir.statemachine_name i ir.statemachine_name current_state_name
  ) ir.final_state;

  Printf.fprintf c_out_channel "\n";
(*-------------------------------------------------------------------------------------*)
  (* Loop that populates the .transitions[] array: *)
  List.iteri (fun i (src_state, event, dst_state) ->
    Printf.fprintf c_out_channel "    %s.transitions[%d] = (Transition){\n" ir.statemachine_name i;
    Printf.fprintf c_out_channel "        .src_state = %s.states[%s],\n" ir.statemachine_name (state_string_identifier src_state);
    Printf.fprintf c_out_channel "        .event     = (Event){.name = \"%s\"},\n" (event_string_identifier event);
    Printf.fprintf c_out_channel "        .dst_state = %s.states[%s]\n" ir.statemachine_name (state_string_identifier dst_state);
    Printf.fprintf c_out_channel "    };\n";
  ) ir.transitions;
  Printf.fprintf c_out_channel "    %s.transition_count = %d;\n" ir.statemachine_name (List.length ir.transitions);
(*-------------------------------------------------------------------------------------*)
  (* The state machine's main-loop that reads user input + call to state_machine_step(...): *)
  Printf.fprintf c_out_channel "    \n#define STATE_MACHINE_RUNNING true\n\n";
  Printf.fprintf c_out_channel "    while (STATE_MACHINE_RUNNING) {\n";
  Printf.fprintf c_out_channel "        printf(\"\\nCurrent state: %%s\\n\", %s.current_state.name);\n" ir.statemachine_name;
  Printf.fprintf c_out_channel "        printf(\"Enter event: \");\n";
  Printf.fprintf c_out_channel "        char entered_event_name[256];\n";
  Printf.fprintf c_out_channel "        while (scanf(\"%%s255\", &entered_event_name) != true) {\n";
  Printf.fprintf c_out_channel "            printf(\"\\nInvalid Input!\\n\");\n";
  Printf.fprintf c_out_channel "            while (getchar() != '\\n');\n";
  Printf.fprintf c_out_channel "        };\n";
  Printf.fprintf c_out_channel "        Event entered_event = (Event){.name = entered_event_name};\n";
  Printf.fprintf c_out_channel "        state_machine_step(&%s, entered_event);\n" ir.statemachine_name;
  Printf.fprintf c_out_channel "    }\n\n";
  Printf.fprintf c_out_channel "    return 0;\n";
  Printf.fprintf c_out_channel "}";
(*-------------------------------------------------------------------------------------*)
  close_out c_out_channel;

  (* Debug message to terminal, not for the .C file: *)
  Printf.printf "\n\nCreated C output file at: %s\n\n" c_file