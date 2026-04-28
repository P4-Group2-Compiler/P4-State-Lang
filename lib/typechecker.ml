open Ast

(* Error handling and setup using location for debugging *)
exception Type_error of string

let highlight source (startpos, endpos) msg =
  let lines = String.split_on_char '\n' source in
  let line_num = startpos.Lexing.pos_lnum in
  let col_start = startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol in
  let col_end   = endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol in
  let line = List.nth lines (line_num - 1) in
  let width = max 1 (col_end - col_start) in
  let caret = String.make col_start ' ' ^ String.make width '^' in
  Printf.sprintf
    "Error at %s; Line %d; Position %d\n%s\n%s\n%s"
    startpos.Lexing.pos_fname
    line_num col_start
    line
    caret
    msg

let type_error ?(loc : (Lexing.position * Lexing.position) option = None) s =
  match loc with
  | None ->
      raise (Type_error s)

  | Some (startpos, endpos) ->
      let source =
        let chan = open_in startpos.Lexing.pos_fname in
        let len = in_channel_length chan in
        let text = really_input_string chan len in
        close_in chan;
        text
      in

      (* Use s directly as the message *)
      let pretty_error = highlight source (startpos, endpos) s in
      raise (Type_error pretty_error)

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
  | Eident {id; loc} -> 
      (try Hashtbl.find env id 
        with Not_found ->
      type_error ~loc:(Some loc) ("Unbound variable: " ^ id)) (* Added location for better debugging *)
  | Ebinop (binop, e1, e2) ->
      let t1 = type_expr env e1 in
      let t2 = type_expr env e2 in
      begin match binop, t1, t2 with
      | (Blt | Ble | Bgt | Bge) ,Tint, Tint -> Tbool
      | _ -> type_error "Comparisons expects 'int : int'"
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