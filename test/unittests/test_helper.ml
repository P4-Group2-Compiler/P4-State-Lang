open P4
open P4.Ast

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