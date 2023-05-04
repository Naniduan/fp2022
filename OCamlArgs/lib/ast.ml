(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type erasabilty =
  | Erasable
  | NonErasable
[@@deriving eq, show]

type argtyp =
  | ASimple
  | AOptional of string
  | ALabeled of string
[@@deriving eq, show]

type paramtyp =
  | ParSimple of string
  | ParOptional of string
  | ParLabeled of string
[@@deriving eq, show]

and typ =
  | TBool
  | TInt
  | TString
  | TRef of typ
  | TTuple of typ list
  | TList of typ
  | TVar of int
  | TFun of erasabilty * argtyp * typ * typ
  | TOpt of typ
  | TUninf
[@@deriving equal]

let a_simple = ASimple
let a_optional name = AOptional name
let a_labeled name = ALabeled name
let par_simple name = ParSimple name
let par_optional name = ParOptional name
let par_labeled name = ParLabeled name
let t_bool = TBool
let t_int = TInt
let t_string = TString
let t_ref typ = TRef typ
let t_tuple types = TTuple types
let t_unit = t_tuple []
let t_list typ = TList typ
let t_var name = TVar name
let t_fun ety argtyp argt rett = TFun (ety, argtyp, argt, rett)
let t_opt typ = TOpt typ
let t_uninf = TUninf

let rec pp_argtyp fmt = function
  | ASimple -> ()
  | ALabeled name -> fprintf fmt "~%s:" name
  | AOptional name -> fprintf fmt "?%s:" name
;;

let rec pp_argtyp_without_tild fmt = function
  | ASimple -> ()
  | ALabeled name -> fprintf fmt "%s:" name
  | AOptional name -> fprintf fmt "?%s:" name
;;

let rec pp_typ fmt = function
  | TBool -> fprintf fmt "bool"
  | TInt -> fprintf fmt "int"
  | TString -> fprintf fmt "string"
  | TRef typ -> fprintf fmt "%a ref" pp_typ typ
  | TTuple types ->
    fprintf
      fmt
      "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt " * ")
         (fun fmt typ -> pp_typ fmt typ))
      types
  | TList typ -> fprintf fmt "%a list" pp_typ typ
  | TVar name -> fprintf fmt "'%n" name
  | TFun (_, argtyp, argt, rett) ->
    fprintf fmt "(%a%a -> %a)" pp_argtyp_without_tild argtyp pp_typ argt pp_typ rett
  | TOpt typ -> fprintf fmt "((%a) option)" pp_typ typ
  | TUninf -> fprintf fmt "(Uninferred)"
;;

type un_op = Neg [@@deriving eq]

let neg = Neg

let pp_un_op fmt = function
  | Neg -> fprintf fmt "-"
;;

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Les
  | Gre
  | Eq
  | Leseq
  | Greeq
  | Neq
  | And
  | Or
  | Con
  | Asgmt
[@@deriving eq]

let add = Add
let sub = Sub
let mul = Mul
let div = Div
let les = Les
let gre = Gre
let eq = Eq
let leseq = Leseq
let greeq = Greeq
let neq = Neq
let and_ = And
let or_ = Or
let con = Con
let asgmt = Asgmt

let pp_bin_op fmt op =
  fprintf
    fmt
    (match op with
     | Add -> "+"
     | Sub -> "-"
     | Mul -> "*"
     | Div -> "/"
     | Les -> "<"
     | Gre -> ">"
     | Eq -> "="
     | Leseq -> "<="
     | Greeq -> ">="
     | Neq -> "!="
     | And -> "&&"
     | Or -> "||"
     | Con -> "::"
     | Asgmt -> ":=")
;;

type const =
  | CInt of int
  | CString of string
  | CBool of bool
  | CEmptyList
  | CNone
[@@deriving eq]

let c_int n = CInt n
let c_string s = CString s
let c_bool b = CBool b
let c_empty_list = CEmptyList
let c_none = CNone

let pp_const fmt = function
  | CInt d -> fprintf fmt "%d" d
  | CString s -> fprintf fmt "%S" s
  | CBool b -> fprintf fmt "%b" b
  | CEmptyList -> fprintf fmt "[]"
  | CNone -> fprintf fmt "None"
;;

type pat =
  | PVal of string
  | PConst of const
  | PTuple of pat list
  | PCons of pat list * pat
  | PSome of pat
[@@deriving eq, show { with_path = false }]

let p_val name = PVal name
let p_const const = PConst const
let p_tuple pats = PTuple pats
let p_unit = p_tuple []
let p_cons pats pat = PCons (pats, pat)
let p_some pat = PSome pat

let rec pp_pat fmt = function
  | PVal s -> fprintf fmt "%s" s
  | PConst c -> pp_const fmt c
  | PTuple l ->
    fprintf fmt "(%a)" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_pat) l
  | PCons (pats, pat) ->
    pp_print_list
      ~pp_sep:(fun _ () -> ())
      (fun fmt p -> fprintf fmt "%a :: " pp_pat p)
      fmt
      pats;
    pp_pat fmt pat
  | PSome pat -> fprintf fmt "(Some (%a))" pp_pat pat
;;

type recty =
  | Recusive
  | Not_recursive
[@@deriving eq]

type dec = recty * string * typ * expr [@@deriving eq]

and expr =
  | EUnop of un_op * expr
  | EBinop of expr * bin_op * expr
  | EApp of argtyp * expr * expr
  | EConst of const
  | EVal of string
  | ETuple of expr list
  | ELet of dec * expr
  | EMatch of expr * (pat * expr) list
  | EFun of paramtyp * typ * expr
  | ESome of expr
  | EParens of expr

let d_dec recty name typ expr = recty, name, typ, expr
let e_parens e = EParens e
let e_un_op op expr = EUnop (op, expr)
let e_bin_op expr1 op expr2 = EBinop (expr1, op, expr2)
let e_app argty expr1 expr2 = EApp (argty, expr1, expr2)
let e_const const = EConst const
let e_val name = EVal name
let e_tuple exprs = ETuple exprs
let e_unit = e_tuple []
let e_let dec expr = ELet (dec, expr)
let e_match scr cases = EMatch (scr, List.map (fun (p, e) -> p, e) cases)
let e_fun paramtyp typ expr = EFun (paramtyp, typ, expr)
let e_some e = ESome e

let pp_paramtyp fmt = function
  | ParSimple name -> fprintf fmt "%s" name
  | ParOptional name -> fprintf fmt "?%s" name
  | ParLabeled name -> fprintf fmt "~%s" name
;;

let rec pp_dec fmt (recty, name, typ, expr) =
  fprintf
    fmt
    "let %s%s : %a = %a"
    (if recty = Recusive then "rec " else "")
    name
    pp_typ
    typ
    pp_expr
    expr

and pp_expr fmt = function
  | EUnop (op, expr) -> fprintf fmt "%a%a" pp_un_op op pp_expr expr
  | EBinop (expr1, op, expr2) ->
    fprintf fmt "%a %a %a" pp_expr expr1 pp_bin_op op pp_expr expr2
  | EApp (argtyp, expr1, expr2) ->
    fprintf fmt "%a %a%a" pp_expr expr1 pp_argtyp argtyp pp_expr expr2
  | EConst c -> pp_const fmt c
  | EVal s -> fprintf fmt "%s" s
  | ETuple l -> pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr fmt l
  | ELet (dec, expr) -> fprintf fmt "%a in\n%a" pp_dec dec pp_expr expr
  | EMatch (scr, pats) ->
    fprintf
      fmt
      "match %a with\n%a"
      pp_expr
      scr
      (pp_print_list (fun fmt (p, e) -> fprintf fmt "| %a -> %a" pp_pat p pp_expr e))
      pats
  | EFun (paramtyp, typ, expr) ->
    fprintf fmt "fun %a : %a -> %a" pp_paramtyp paramtyp pp_typ typ pp_expr expr
  | ESome e -> fprintf fmt "Some %a" pp_expr e
  | EParens e -> fprintf fmt "(%a)" pp_expr e
;;

type program = dec list [@@deriving eq]

let pp_program fmt program =
  pp_print_list (fun fmt dec -> fprintf fmt "%a\n;;\n" pp_dec dec) fmt program
;;
