open P4
open Typechecker


(* *********************************************************** *)
(*                       FUN TYPE_CONST                        *)
(* *********************************************************** *)
let test_type_const_happy () =
    let open P4.Ast in
    let open Typechecker in

    Alcotest.(check bool) "Int constant"
        true (type_const (Cint 42) = Tint);
    
    Alcotest.(check bool) "Bool constant"
        true (type_const (Cbool true) = Tbool)

(* *********************************************************** *)
(*                       FUN TYPE_EXPR                         *)
(* *********************************************************** *)        
(* *********************************************************** *)
(*                     FUN TYPE_EXPR_ECST                      *)
(* *********************************************************** *) 
let test_type_expr_const_happy () =
    let open P4.Ast in
    let open Typechecker in

    (* type_expr takes env as argument, so we create it before calling type_expr*)
    let env = Hashtbl.create 4 in 

    Alcotest.(check bool) "Ecst expr"
        (* type_expr takes the function of type_const, so we explicitly give it arguments
        to evaluate, before returning a bool *)
        true (type_expr env (Ecst (Cint 34)) = Tint)

(* *********************************************************** *)
(*                   FUN TYPE_EXPR_Eident                      *)
(* *********************************************************** *) 
let test_type_expr_ident_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in
    (* Eident takes the record {id;loc}, so they need to be mocked *)
    (* First id *)
    Hashtbl.add env "x" Tint;

    (* Then loc *)
    let dummy_pos = {
        Lexing.pos_fname = "";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0;
    } in
    let dummy_loc = (dummy_pos, dummy_pos) in

    let expr = Eident { id = "x"; loc = dummy_loc } in

    Alcotest.(check bool) "Eident expr"
        true (type_expr env expr = Tint)

(* *********************************************************** *)
(*                 FUN TYPE_EXPR_EBINOP_BOOL                   *)
(* *********************************************************** *)
let test_type_expr_ebinop_band_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Band, Ecst (Cbool true), Ecst (Cbool false)) in

    Alcotest.(check bool) "Ebinop Band expr"
        true (type_expr env expr = Tbool)

let test_type_expr_ebinop_bor_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Bor, Ecst (Cbool true), Ecst (Cbool false)) in

    Alcotest.(check bool) "Ebinop Bor expr"
        true (type_expr env expr = Tbool)

(* *********************************************************** *)
(*                 FUN TYPE_EXPR_EBINOP_ARITH                  *)
(* *********************************************************** *)
let test_type_expr_ebinop_badd_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Badd, Ecst (Cint 42), Ecst (Cint 42)) in

    Alcotest.(check bool) "Ebinop Badd expr"
        true (type_expr env expr = Tint)

let test_type_expr_ebinop_bsub_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Bsub, Ecst (Cint 42), Ecst (Cint 42)) in

    Alcotest.(check bool) "Ebinop Bsub expr"
        true (type_expr env expr = Tint)

let test_type_expr_ebinop_bmul_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Bmul, Ecst (Cint 42), Ecst (Cint 42)) in

    Alcotest.(check bool) "Ebinop Bmul expr"
        true (type_expr env expr = Tint)

(* *********************************************************** *)
(*                 FUN TYPE_EXPR_EBINOP_COMP                   *)
(* *********************************************************** *)
let test_type_expr_ebinop_beq_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Beq, Ecst (Cint 42), Ecst (Cint 42)) in

    Alcotest.(check bool) "Ebinop Beq expr"
        true (type_expr env expr = Tbool)

let test_type_expr_ebinop_bneq_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Bneq, Ecst (Cint 42), Ecst (Cint 42)) in

    Alcotest.(check bool) "Ebinop Bneq expr"
        true (type_expr env expr = Tbool)

let test_type_expr_ebinop_blt_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Blt, Ecst (Cint 42), Ecst (Cint 42)) in

    Alcotest.(check bool) "Ebinop Blt expr"
        true (type_expr env expr = Tbool)

let test_type_expr_ebinop_ble_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Ble, Ecst (Cint 42), Ecst (Cint 42)) in

    Alcotest.(check bool) "Ebinop Ble expr"
        true (type_expr env expr = Tbool)

let test_type_expr_ebinop_bgt_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Bgt, Ecst (Cint 42), Ecst (Cint 42)) in

    Alcotest.(check bool) "Ebinop Bgt expr"
        true (type_expr env expr = Tbool)

let test_type_expr_ebinop_bge_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 4 in

    let expr =
        Ebinop (Bge, Ecst (Cint 42), Ecst (Cint 42)) in

    Alcotest.(check bool) "Ebinop Bge expr"
        true (type_expr env expr = Tbool)

(* *********************************************************** *)
(*                      FUN TYPE_OPERATION                     *)
(* *********************************************************** *)
(* *********************************************************** *)
(*                         HAPPY TESTS                         *)
(* *********************************************************** *)
let test_type_operation_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 10 in
    Hashtbl.add env "x" Tint;

    let expr = Ecst (Cint 42) in
    let dummy_pos = {
        Lexing.pos_fname = "";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0;
    } in
    let dummy_loc = (dummy_pos, dummy_pos) in
    let id = { id = "x"; loc = dummy_loc } in
    let op = Do (id, expr) in

    Alcotest.(check bool) "type operation"
        true (type_operation env op; true)

(* *********************************************************** *)
(*                          SAD TESTS                          *)
(* *********************************************************** *)
let test_type_operation_unbound_sad () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 10 in

    let expr = Ecst (Cint 42) in
    let dummy_pos = {
        Lexing.pos_fname = "<test>"; (* <test> is some dummy file name needed for not breaking ocaml *)
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0;
    } in
    let dummy_loc = (dummy_pos, dummy_pos) in
    let id = { id = "x"; loc = dummy_loc } in
    let op = Do (id, expr) in

    Alcotest.check_raises "unbound variable"
        (Type_error "Unbound variable in operation: x")
        (fun () -> type_operation env op)

let test_type_operation_mistype_sad () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 10 in
    Hashtbl.add env "x" Tbool;

    let expr = Ecst (Cint 42) in
    let dummy_pos = {
        Lexing.pos_fname = "<test>";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0;
    } in
    let dummy_loc = (dummy_pos, dummy_pos) in
    let id = { id = "x"; loc = dummy_loc } in
    let op = Do (id, expr) in

    Alcotest.check_raises "mistyped variable"
        (Type_error "Type mismatch in operation assignment to variable: x")
        (fun () -> type_operation env op)

(* *********************************************************** *)
(*                     FUN TYPE_TRANSITION                     *)
(* *********************************************************** *)
(* *********************************************************** *)
(*                        HAPPY TESTS                          *)
(* *********************************************************** *)

let test_type_transition_no_guard_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 10 in
    Hashtbl.add env "x" Tint;

    let expr = Ecst (Cint 42) in
    let dummy_pos = {
        Lexing.pos_fname = "<test>";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0;
    } in
    let dummy_loc = (dummy_pos, dummy_pos) in
    let id = { id = "x"; loc = dummy_loc } in
    let op = Do (id, expr) in
    let tr = Transition (Event "ev", None, State "A", [op]) in

    Alcotest.(check bool) "transition without guard succeeds"
        true (type_transition env tr; true)

let test_type_transition_guard_happy () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 10 in
    Hashtbl.add env "x" Tint;
    Hashtbl.add env "y" Tint;

    let expr = Ecst (Cint 42) in
    let dummy_pos = {
        Lexing.pos_fname = "<test>";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0;
    } in
    let dummy_loc = (dummy_pos, dummy_pos) in
    let id_x = { id = "x"; loc = dummy_loc } in
    let id_y = { id = "y"; loc = dummy_loc } in
    
    let op = Do (id_x, expr) in
    let guard = Ebinop (Bgt, Eident id_x, Eident id_y) in
    let tr = Transition (Event "ev", Some guard , State "A", [op]) in

    Alcotest.(check bool) "transition with guard succeeds"
        true (type_transition env tr; true)

(* *********************************************************** *)
(*                          SAD TESTS                          *)
(* *********************************************************** *)

let test_type_transition_no_guard_ops_sad () =
    let open P4.Ast in
    let open Typechecker in

    let env = Hashtbl.create 10 in
    Hashtbl.add env "x" Tint;

    let expr = Ecst (Cint 42) in
    let dummy_pos = {
        Lexing.pos_fname = "";
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0;
    } in
    let dummy_loc = (dummy_pos, dummy_pos) in
    let id = { id = "x"; loc = dummy_loc } in
    let op = Do (id, expr) in
    let tr = Transition (Event "ev", None, State "A", [op]) in

    Alcotest.(check bool) "transition without guard succeeds"
        true (type_transition env tr; true)

let () =
  Alcotest.run "Typechecking Tests" [
    "Typechecking", [
    Alcotest.test_case "type_const"      `Quick test_type_const_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_const_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_ebinop_band_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_ebinop_bor_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_ebinop_badd_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_ebinop_bsub_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_ebinop_bmul_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_ebinop_beq_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_ebinop_bneq_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_ebinop_blt_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_ebinop_ble_happy;
    Alcotest.test_case "type_expr"       `Quick test_type_expr_ebinop_bgt_happy;
    Alcotest.test_case "type_operation"  `Quick test_type_operation_happy;
    Alcotest.test_case "type_operation"  `Quick test_type_operation_unbound_sad;
    Alcotest.test_case "type_operation"  `Quick test_type_operation_mistype_sad;
    Alcotest.test_case "type_transition" `Quick test_type_transition_guard_happy;
    Alcotest.test_case "type_transition" `Quick test_type_transition_no_guard_happy;
    ];
  ]