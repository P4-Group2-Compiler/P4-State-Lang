open P4
open P4.Ast
open P4.Semantic

(* Unlike Junit(JS) or Juniper(Java), Alcotest library needs to define/write its own
helper functions to assert equality of testing values. There is no inherent 'assertEqual()'
or similar *)

(* Helper function for collect_transitions *)
let pp_transition fmt ((src:state), (evt:event), (guard:expr option), (dst:state), (ops:operation list)) =
  Format.fprintf fmt "(%s, %s, %s, %s, %d ops)"
    (Semantic.state_to_string src)
    (Semantic.event_to_string evt)
    (match guard with None -> "None" | Some _ -> "Some")
    (Semantic.state_to_string dst)
    (List.length ops)

let transition_equal
    ((s1:state), (e1:event), (g1:expr option), (d1:state), (o1:operation list))
    ((s2:state), (e2:event), (g2:expr option), (d2:state), (o2:operation list)) =
  s1 = s2 && e1 = e2 && g1 = g2 && d1 = d2 && o1 = o2


let transition_testable =
  Alcotest.testable pp_transition transition_equal

(* Helper for type warning in semantic *)
let pp_warning fmt = function
  | TooManyStates n ->
      Format.fprintf fmt "TooManyStates(%d)" n

  | UnreachableFinalState st ->
      Format.fprintf fmt "UnreachableFinalState(%s)"
        (Semantic.state_to_string st)

  | DuplicateTransitions (src, evt, dst) ->
      Format.fprintf fmt "DuplicateTransitions(%s, %s, %s)"
        (Semantic.state_to_string src)
        (Semantic.event_to_string evt)
        (Semantic.state_to_string dst)

let equal_warning w1 w2 =
  match w1, w2 with
  | TooManyStates n1, TooManyStates n2 ->
      n1 = n2

  | UnreachableFinalState s1, UnreachableFinalState s2 ->
      s1 = s2

  | DuplicateTransitions (s1, e1, d1),
    DuplicateTransitions (s2, e2, d2) ->
      s1 = s2 && e1 = e2 && d1 = d2

  | _ -> false


let warning_testable =
  Alcotest.testable pp_warning equal_warning

(* Helper for get_start_states *)
let pp_state fmt st =
  Format.fprintf fmt "%s" (Semantic.state_to_string st)

let state_equal s1 s2 = s1 = s2

let state_testable = Alcotest.testable pp_state state_equal

(* Helper for collect_g_variables *)
(* Pretty-print for var_decl *)
let pp_var_decl fmt = function
  | Var_decl (name, value) ->
    Format.fprintf fmt "Var_decl(%s, %d)" name value

(* Equality of var_decl *)
let var_decl_equal v1 v2 =
  match v1, v2 with
  | Var_decl (n1, v1), Var_decl (n2, v2) ->
      n1 = n2 && v1 = v2
  
let var_decl_testable =
  Alcotest.testable pp_var_decl var_decl_equal

let var_decl_list_testable =
  Alcotest.(list var_decl_testable)

(* Helper for check_valid_transition *)
let expect_semantic_error f =
  try
    f (); (* run the argument function *)
    Alcotest.fail "expected Semantic_error, but no exception was raised" (* we expect an error, therefore test FAILS if none is raised *) 
  with
  | Semantic.Semantic_error _ -> ()   (* The correct error constructer returned arbitrary error; SUCCESS! *)
  | ex -> (* Any other error than what we want, fails the test *)
      Alcotest.failf "expected Semantic_error, but got %s"
        (Printexc.to_string ex)
