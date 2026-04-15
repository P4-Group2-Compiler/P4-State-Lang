open Ast

let state_string_identifier = function
  | Ast.State state_string -> state_string

let event_string_identifier = function
  | Ast.Event event_string -> event_string

(* Collects events by looping over all events contained in each transition, in each state *)
let collect_events program_ast =
  (* Hashtable to avoid collecting duplicate events contained in multiple states: *)
  let seen_events = Hashtbl.create 32 in (* Hashtbl limited to 32 events *)
  let event_list = ref [] in
  List.iter (fun state ->
    (* '_' to ignore the state *)
    List.iter (fun (Ast.Transition (event, _)) ->
      let current_event_name = event_string_identifier event in
      if not (Hashtbl.mem seen_events current_event_name) then begin
        Hashtbl.add seen_events current_event_name ();
        event_list := current_event_name :: !event_list
      end
    ) state.transitions
    
  ) program_ast.states;

  !event_list

let generate_c_code ast =
  let events = collect_events ast in
  (* Looping through the states for a match between "state_type" and "Start" to find the start state *)
  let start_name =
    match List.find_opt (fun state -> state.state_type = "Start") ast.states with
    | Some state -> state_string_identifier state.name
    | None -> state_string_identifier (List.hd ast.states).name
  in

  Printf.printf "#include <stdio.h>\n";
  Printf.printf "#include \"state_machine.h\"\n\n";

  Printf.printf "typedef enum {\n    ";
  List.iter (fun state -> Printf.printf "%s,\n    " (state_string_identifier state.name)) ast.states;
  Printf.printf "\n    STATE_COUNT\n} StateID_t;\n\n";

  Printf.printf "typedef enum {\n    ";
  List.iter (fun event -> Printf.printf "%s,\n    " event) events;
  Printf.printf "\n    EVENT_COUNT\n} EventID_t;\n\n";

  Printf.printf "int main(void) {\n";
  Printf.printf "    StateMachine %s = {0};\n" ast.machine_name;
  Printf.printf "    %s.state_count   = STATE_COUNT;\n" ast.machine_name;
  Printf.printf "    %s.start_state   = %s;\n" ast.machine_name start_name;
  Printf.printf "    %s.current_state = %s.start_state;\n\n" ast.machine_name ast.machine_name;

  List.iter (fun state ->
    let current_state_name = state_string_identifier state.name in
    let is_start = if state.state_type = "Start" then "true" else "false" in
    Printf.printf "    %s.states[%s] = (State){\n" ast.machine_name current_state_name;
    Printf.printf "        .identifier       = %s,\n" current_state_name;
    Printf.printf "        .is_start         = %s,\n" is_start;
    Printf.printf "        .transition_count = %d,\n" (List.length state.transitions);
    Printf.printf "        .transitions = {\n";
    List.iter (fun (Ast.Transition (event, target)) ->
      Printf.printf "            { %s, %s },\n" (event_string_identifier event) (state_string_identifier target)
    ) state.transitions;
    Printf.printf "        },\n";
    Printf.printf "    };\n\n"
  ) ast.states;

  Printf.printf "    while (1) {\n";
  Printf.printf "        printf(\"Current state: %%d\\n\", %s.current_state);\n" ast.machine_name;
  Printf.printf "        printf(\"Enter event: \");\n";
  Printf.printf "        int e; scanf(\"%%d\", &e);\n";
  Printf.printf "        state_machine_step(&%s, e);\n" ast.machine_name;
  Printf.printf "    }\n";
  Printf.printf "    return 0;\n";
  Printf.printf "}"