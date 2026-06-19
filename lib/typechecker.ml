open Ast
open Semantic

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
        match open_in startpos.Lexing.pos_fname with
        | exception Sys_error _ -> None
        | chan ->
            let len = in_channel_length chan in
            let text = really_input_string chan len in
            close_in chan;
            Some text
      in
      (match source with
      | None -> raise (Type_error s)
      | Some src ->
          let pretty_error = highlight src (startpos, endpos) s in
          raise (Type_error pretty_error))
    

(* primary/primitive types *)
type ty =
| Tint
| Tbool

(* Type environment for handling typechecking 
like ctx (context) in WHILE lang *)
type type_env = (string, ty) Hashtbl.t

(* Checking constants. We check for arbitrary values _,
since we dont care about the value, only the type *)
let type_const = function
  | Cint _ -> Tint
  | Cbool _ -> Tbool

(* Checking expressions*)
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
    | (Badd | Bsub | Bmul), Tint, Tint ->
        Tint
    | (Beq | Bneq | Blt | Ble | Bgt | Bge), Tint, Tint ->
        Tbool
    | (Band | Bor), Tbool, Tbool ->
        Tbool
    | (Band | Bor), _, _ ->
        type_error "Boolean operators expect bool : bool"
    | (Badd | Bsub | Bmul), _, _ ->
        type_error "Arithmetic operators expect int : int"
    | (Beq | Bneq | Blt | Ble | Bgt | Bge), _, _ ->
        type_error "Comparison operators expect int : int"
    end
    
(* Checking transition operations *)
let type_operation env = function
  | Do (id, expr) ->
      let var_type =
        try Hashtbl.find env id.id
        with Not_found ->
          type_error ~loc:(Some id.loc) ("Unbound variable in operation: " ^ id.id)
      in

      let expr_type = type_expr env expr in

      if var_type <> expr_type then
        type_error ~loc:(Some id.loc)
          ("Type mismatch in operation assignment to variable: " ^ id.id)

(* Checking transitions *)
let type_transition env = function
  | Transition (_ev, None, _st, ops) ->
      List.iter (type_operation env) ops

  | Transition (_ev, Some guard, _st, ops) ->
      begin
        match type_expr env guard with
        | Tbool -> ()
        | _ -> type_error "Guard expression must have type bool"
      end;

      List.iter (type_operation env) ops

(* Checking the program *)
let initial_env () = Hashtbl.create 16

let type_program statemachine =
  let env = initial_env () in
  List.iter (fun (Var_decl (name, _)) ->
    Hashtbl.add env name Tint) statemachine.g_variables;
  List.iter (fun (src, event, guard, dest, op) ->
    type_transition env (Transition (event, guard, dest, op))) statemachine.transitions