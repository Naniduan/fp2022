(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser
open Format
module BindMap = Map.Make (String)
module BindSet = Set.Make (String)

module type FailBimapMonad = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val bimap : ('a, 'e) t -> ok:('a -> 'b) -> err:('e -> 'b) -> 'b
end

module Interpret (M : FailBimapMonad) : sig
  type value
  and r_ok
  and r_err

  val pp_r_ok : formatter -> r_ok -> unit
  val pp_r_err : formatter -> r_err -> unit
  val run : dec list -> (r_ok, r_err) M.t
end = struct
  open M

  let ( let* ) m f = bind m ~f

  type env = value BindMap.t

  and value =
    | VInt of int
    | VString of string
    | VBool of bool
    | VTuple of value list
    | VList of value list
    | VFun of ((argtyp * value, r_err) t Lazy.t -> (value, r_err) t)
    | VNone
    | VSome of value

  and r_err =
    | Div0
    | Cmp_fun
    | Unbound of string
    | Incorrect_typ of value
    | Non_exhaustive of pat list

  let v_int n = VInt n
  let v_string s = VString s
  let v_bool b = VBool b
  let v_tuple l = VTuple l
  let v_list l = VList l
  let v_lfun f = VFun f
  let v_fun f = v_lfun (fun x -> Lazy.force x >>= f)
  let v_fun2 f = v_fun (fun x -> return (v_fun (fun y -> f (x, y))))
  let v_some s = VSome s

  let rec pp_value fmt = function
    | VInt n -> fprintf fmt "%d" n
    | VString s -> fprintf fmt "%S" s
    | VBool b -> fprintf fmt "%b" b
    | VTuple l ->
      fprintf
        fmt
        "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_value)
        l
    | VList l ->
      fprintf
        fmt
        "[%a]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_value)
        l
    | VFun _ -> fprintf fmt "<fun>"
    | VNone -> fprintf fmt "None"
    | VSome s -> fprintf fmt "(Some %a)" pp_value s
  ;;

  let pp_r_err fmt = function
    | Unbound s -> fprintf fmt "Unbound value %s" s
    | Incorrect_typ v -> fprintf fmt "Value %a has incorrect type" pp_value v
    | Non_exhaustive ps ->
      fprintf fmt "This pattern-matching is not exhaustive:\n%a" (pp_print_list pp_pat) ps
    | Div0 -> fprintf fmt "Division by zero"
    | Cmp_fun -> fprintf fmt "Compare: functional value"
  ;;

  let pp_env fmt env =
    BindMap.iter (fun k v -> Format.fprintf fmt "%s=%a\n" k pp_value v) env
  ;;

  type r_ok_elm = string * (typ * value)

  let pp_r_ok_elm fmt (name, (ty, value)) =
    Format.fprintf fmt "val %s : %a = %a" name pp_typ ty pp_value value
  ;;

  type r_ok = r_ok_elm list

  let pp_r_ok = pp_print_list ~pp_sep:pp_force_newline pp_r_ok_elm

  let rec cmp_value = function
    | VInt x, VInt y -> return (Int.compare x y)
    | VString x, VString y -> return (String.compare x y)
    | VBool x, VBool y -> return (Bool.compare x y)
    | VTuple (x :: xs), VTuple (y :: ys) | VList (x :: xs), VList (y :: ys) ->
      let* cmp = cmp_value (x, y) in
      (match cmp with
       | 0 -> cmp_value (VTuple xs, VTuple ys)
       | _ -> return cmp)
    | VTuple [], VTuple (_ :: _) | VList [], VList (_ :: _) -> return (-1)
    | VTuple (_ :: _), VTuple [] | VList (_ :: _), VList [] -> return 1
    | VTuple [], VTuple [] | VList [], VList [] -> return 0
    | VFun _, VFun _ -> fail Cmp_fun
    | x, y -> fail (Incorrect_typ (VTuple [ x; y ]))
  ;;

  let add_val name value env = BindMap.add name value env

  let rec case_env value ptrn =
    match ptrn, value with
    | PVal s, _ -> Some (return (add_val s value BindMap.empty))
    | PConst (CInt c), VInt v when c = v -> Some (return BindMap.empty)
    | PConst (CInt _), VInt _ -> None
    | PConst (CString c), VString v when c = v -> Some (return BindMap.empty)
    | PConst (CString _), VString _ -> None
    | PConst (CBool c), VBool v when c = v -> Some (return BindMap.empty)
    | PConst (CBool _), VBool _ -> None
    | PConst CEmptyList, VList [] -> Some (return BindMap.empty)
    | PConst CEmptyList, VList _ -> None
    | PTuple ps, VTuple vs ->
      (match vs, ps with
       | [], [] -> Some (return BindMap.empty)
       | v :: vs, p :: ps ->
         mrg_case_envs (case_env v p) (fun () -> case_env (VTuple vs) (PTuple ps))
       | _ -> Some (fail (Incorrect_typ value)))
    | PCons (ps, pl), VList vs ->
      (match vs, ps with
       | (_ : value list), [] -> case_env value pl
       | v :: vs, p :: ps ->
         mrg_case_envs (case_env v p) (fun () -> case_env (VList vs) (PCons (ps, pl)))
       | _ -> None)
    | PConst CNone, VNone -> Some (return BindMap.empty)
    | PSome p, VSome v -> case_env v p
    | PSome _, VNone -> None
    | PConst CNone, VSome _ -> None
    | _ ->
      printf "%a ~~~ %a" pp_pat ptrn pp_value value;
      Some (fail (Incorrect_typ value))

  and mrg_case_envs env1 env2 =
    Option.bind env1 (fun env1 ->
      bimap
        env1
        ~ok:(fun env1 ->
          Option.map
            (fun env2 -> env2 >>| fun env2 -> BindMap.add_seq (BindMap.to_seq env2) env1)
            (env2 ()))
        ~err:(fun err -> Some (fail err)))
  ;;

  let lookup_val name env =
    match BindMap.find_opt name env with
    | None -> fail (Unbound name)
    | Some r -> return r
  ;;

  let rec e_fun_eval (argtyp, arg) ?(mem_args = []) prmtyp body env =
    let eval_body env =
      List.fold_left
        (fun acc arg ->
          acc
          >>= fun acc ->
          match acc with
          | VFun fv -> fv (lazy (return arg))
          | _ -> fail (Incorrect_typ acc))
        (eval body env)
        mem_args
    in
    match prmtyp, argtyp with
    | ParSimple prm, ASimple -> eval_body (add_val prm arg env)
    | (ParOptional prm1, AOptional prm2 | ParLabeled prm1, ALabeled prm2) when prm1 = prm2
      -> eval_body (add_val prm1 arg env)
    | _ ->
      return
        (v_fun (fun next_arg ->
           e_fun_eval next_arg ~mem_args:((argtyp, arg) :: mem_args) prmtyp body env))

  and eval expr env =
    match expr with
    | EUnop (un_op, expr1) ->
      let* value1 = eval expr1 env in
      (match un_op, value1 with
       | Neg, VInt n -> return (VInt (-n))
       | _ -> fail (Incorrect_typ value1))
    | EBinop (expr1, un_op, expr2) ->
      let* value1 = eval expr1 env in
      let* value2 = eval expr2 env in
      (match value1, un_op, value2 with
       | VInt x, Add, VInt y -> return (VInt (x + y))
       | VInt x, Sub, VInt y -> return (VInt (x - y))
       | VInt x, Mul, VInt y -> return (VInt (x * y))
       | VInt x, Div, VInt y ->
         (match y with
          | 0 -> fail Div0
          | _ -> return (VInt (x / y)))
       | VInt x, Les, VInt y -> return (VBool (x < y))
       | VInt x, Gre, VInt y -> return (VBool (x > y))
       | VInt x, Eq, VInt y -> return (VBool (x = y))
       | VInt x, Leseq, VInt y -> return (VBool (x <= y))
       | VInt x, Greeq, VInt y -> return (VBool (x >= y))
       | VInt x, Neq, VInt y -> return (VBool (x != y))
       | VBool x, And, VBool y -> return (VBool (x && y))
       | VBool x, Or, VBool y -> return (VBool (x || y))
       | x, Con, VList y -> return (VList (x :: y))
       | _ -> fail (Incorrect_typ (VTuple [ value1; value2 ])))
    | EVal name -> lookup_val name env
    | EConst (CInt n) -> return (v_int n)
    | EConst (CString s) -> return (v_string s)
    | EConst (CBool b) -> return (v_bool b)
    | EConst CEmptyList -> return (v_list [])
    | EApp (argtyp, fn, arg) ->
      eval fn env
      >>= (function
      | VFun fv -> fv (lazy (eval arg env >>| fun arg -> argtyp, arg))
      | v -> fail (Incorrect_typ v))
    | ETuple l -> all (List.map (fun e -> eval e env) l) >>| v_tuple
    | ELet (dec, expr) -> add_dec dec env >>= eval expr
    | EMatch (scr, cases) ->
      eval scr env
      >>= fun value ->
      (match
         List.find_map
           (fun (ptrn, body) -> Option.map (fun env -> env, body) (case_env value ptrn))
           cases
       with
       | Some (case_env, body) ->
         case_env
         >>= fun case_env -> eval body (BindMap.add_seq (BindMap.to_seq case_env) env)
       | None -> fail (Non_exhaustive (List.map (fun (ptrn, _) -> ptrn) cases)))
    | EFun (prmtyp, _, body) ->
      return (v_fun (fun (argtyp, arg) -> e_fun_eval (argtyp, arg) prmtyp body env))
    | ESome e -> eval e env >>| v_some
    | EConst CNone -> return VNone
    | EParens e -> eval e env

  and add_dec dec env =
    let recty, name, typ, expr = dec in
    if recty = Recusive
    then (
      match expr with
      | EFun (prmtyp, _, body) ->
        let rec eval_self prm_v =
          e_fun_eval prm_v prmtyp body (BindMap.add name (v_fun eval_self) env)
        in
        return (BindMap.add name (v_fun eval_self) env)
      | _ -> add_dec (Not_recursive, name, typ, expr) env)
    else eval expr env >>| fun v -> BindMap.add name v env
  ;;

  let run p =
    List.fold_left
      (fun acc dec ->
        let _, name, t, _ = dec in
        acc
        >>= fun (env, vals) ->
        add_dec dec env
        >>= fun env ->
        lookup_val name env
        >>| fun v -> env, (name, (t, v)) :: List.remove_assoc name vals)
      (return (BindMap.empty, []))
      p
    >>| fun (_, res) -> List.rev res
  ;;
end

module InterpretResult = Interpret (struct
  include Base.Result

  let bimap res ~ok ~err =
    match res with
    | Ok v -> ok v
    | Error v -> err v
  ;;
end)