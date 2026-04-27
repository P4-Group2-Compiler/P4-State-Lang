open Ast

(* Error handling *)
exception Type_error of string
let type_error s = raise (Type_error s)

(* primary/primitive(?) types *)
type ty =
| Tint
| Tbool
| Tstring

(*
Type environment for handling typechecking 
like ctx (context) in WHILE lang
*)
type type_env = (string, ty) Hashtbl.t

(* Checking constants. We check for arbitrary values _,
since we dont care about the value, only the type
*)
let type_const = function
  | Cint _ -> Tint
  | Cbool _ -> Tbool
  | Cstring _ -> Tstring

(* Checking expressions. Derived from WHILE, may or may not be appropriate *)
let rec type_expr (env : type_env) = function
  | Ecst const -> type_const const
  | Eident {id} -> 
      (try Hashtbl.find env id 
      with Not_found ->
        type_error ("Unbound variable_ : " ^ id))
  | Ebinop (binop, e1, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      begin match binop, t1,   t2 with
      | (Blt (*| Ble | Bgt | Bge*)) ,Tint, Tint -> Tbool
      | _ -> type_error "Type error: < expects 'int < int'"
      end

(* Checking transitions *)
let type_transition env = function
  | Transition (_ev, None, _st) -> ()
  | Transition (_ev, Some guard, _st) ->
      match type_expr env guard with
        | Tbool -> ()
        | _ -> type_error "Guard expression must have type bool"

(* Checking state declarations *)
let type_state_decl env st =
  List.iter (type_transition env) st.transitions

(* Checking the program *)
let initial_env () = Hashtbl.create 16

let type_program p =
  let env = initial_env () in
  List.iter (fun (Var_decl (name, _)) ->
    Hashtbl.add env name Tint) p.variables;
  List.iter (type_state_decl env) p.states