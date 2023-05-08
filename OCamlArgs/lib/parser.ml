(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a
let pspace = skip_while is_space
let pspace1 = take_while1 is_space
let ptoken p = pspace *> p
let ptoken1 p = pspace1 *> p
let pstr s = ptoken @@ string s
let pstr1 s = ptoken1 @@ string s
let ppars p = pstr "(" *> p <* pstr ")"

let lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let letter ch = lower ch || upper ch

let digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let keyword = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "let"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "match"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with" -> true
  | _ -> false
;;

let pid =
  ptoken
  @@ lift2
       (fun hd tl -> Char.escaped hd ^ tl)
       (satisfy (fun ch -> ch = '_' || lower ch))
       (take_while (fun ch -> ch = '_' || letter ch || digit ch))
  >>= fun s ->
  if keyword s
  then fail "Parsing error: keyword reserved"
  else if s = "_"
  then fail "Parsing error: wildcard `_` isn't supported"
  else return s
;;

let ptupple pelm = ppars @@ sep_by (pstr ",") pelm
let psign = choice [ pstr "+" *> return 1; pstr "-" *> return (-1); pstr "" *> return 1 ]

let pcint =
  ptoken
  @@ lift2
       (fun sign v -> c_int (sign * v))
       psign
       (take_while digit
       >>= fun s ->
       match int_of_string_opt s with
       | Some x -> return x
       | None -> fail "invalid int")
;;

let pcstring =
  c_string <$> ptoken @@ (string "\"" *> take_while (fun ch -> ch != '"')) <* string "\""
;;

let pcnone = pstr "None" *> return c_none
let pcbool = c_bool <$> (pstr "true" *> return true <|> pstr "false" *> return false)
let pcnil = pstr "[]" *> return c_empty_list
let pconst = choice [ pcint; pcstring; pcbool; pcnil; pcnone ]
let ppval = p_val <$> pid
let ppconst = p_const <$> pconst
let psome p = pstr "Some" *> ptoken1 p

let ppat =
  fix
  @@ fun ppatrn ->
  let term = choice [ ppars ppatrn; ppconst; ppval ] in
  let term = choice [ p_some <$> pstr "Some" *> ptoken1 term; term ] in
  let term =
    lift2
      (fun ptrn ptrns ->
        match List.rev ptrns with
        | [] -> ptrn
        | hd :: tl -> p_cons (ptrn :: List.rev tl) hd)
      term
      (many (pstr "::" *> term))
  in
  (fun l ->
    match l with
    | [ p ] -> p
    | _ -> p_tuple l)
  <$> sep_by1 (pstr ",") term
  <|> pstr "()" *> return p_unit
;;

let pdec_base pexpr =
  pstr "let"
  *> lift3
       (fun is_rec name expr -> d_dec is_rec name t_uninf expr)
       (pstr1 "rec" *> return Recursive <|> return Not_recursive)
       (ptoken1 pid)
       (pstr "=" *> pexpr)
;;

let pcase pkey pexpr = lift2 (fun k v -> k, v) (pstr "|" *> pkey <* pstr "->") pexpr
let peconst = e_const <$> pconst
let peval = e_val <$> pid
let pelet pexpr = lift2 e_let (pdec_base pexpr) (pstr1 "in" *> ptoken1 pexpr)

let pematch pexpr =
  lift2 e_match (pstr "match" *> ptoken1 pexpr <* pstr1 "with") (many1 @@ pcase ppat pexpr)
;;

let parg =
  choice
    [ (fun name -> par_labeled name) <$> pstr "~" *> pid
    ; (fun name -> par_optional name) <$> pstr "?" *> pid
    ; (fun name -> par_simple name) <$> pid
    ]
;;

let pefun pexpr =
  pstr "fun" *> lift2 (fun s e -> e_fun s t_uninf e) (ptoken1 parg <* pstr1 "->") pexpr
;;

let peapp expr1 =
  lift3
    (fun f aty arg -> e_app aty f arg)
    expr1
    (choice
       [ (fun name -> a_labeled name) <$> (pstr1 "~" *> pid <* string ":")
       ; (fun name -> a_optional name) <$> (pstr1 "?" *> pid <* string ":")
       ; return a_simple
       ])
    (ptoken1 expr1)
;;

let binop_from_str = function
  | "+" -> return Add
  | "*" -> return Mul
  | "-" -> return Sub
  | "/" -> return Div
  | ">" -> return Gre
  | "<" -> return Les
  | "=" -> return Eq
  | ">=" -> return Greeq
  | "<=" -> return Leseq
  | "!=" -> return Neq
  | "::" -> return Con
  | "&&" -> return And
  | "||" -> return Or
  | op -> fail ("Unknown binop: " ^ op)
;;

let pebinop chain1 term binops =
  chain1
    term
    (choice (List.map pstr binops)
    >>= fun op -> binop_from_str op >>| fun op expr1 expr2 -> e_bin_op expr1 op expr2)
;;

let pelbinop = pebinop chainl1
let perbinop = pebinop chainr1

let pexpr =
  fix (fun pexpr ->
    let term =
      choice [ pstr "()" *> return e_unit; e_parens <$> ppars pexpr; peconst; peval ]
    in
    let parg =
      choice
        [ pstr "~"
          *> lift2 (fun name value -> a_labeled name, value) (pid <* pstr ":") term
        ; pstr "?"
          *> lift2 (fun name value -> a_optional name, value) (pid <* pstr ":") term
        ; (term >>| fun value -> a_simple, value)
        ]
    in
    let term =
      choice
        [ lift2
            (fun f args ->
              List.fold_left (fun expr (arg, value) -> e_app arg expr value) f args)
            term
            (many (ptoken1 parg))
        ; e_some <$> pstr "Some" *> ptoken1 term
        ]
    in
    let term =
      lift2
        (fun l expr -> List.fold_left (fun expr _ -> e_un_op neg expr) expr l)
        (many (pstr "-"))
        term
    in
    let term = pelbinop term [ "*"; "/" ] in
    let term = pelbinop term [ "+"; "-" ] in
    let term = perbinop term [ "::" ] in
    let term = pelbinop term [ "!="; "="; "<="; "<"; ">="; ">" ] in
    let term = perbinop term [ "&&" ] in
    let term = perbinop term [ "||" ] in
    let term =
      (fun l ->
        match l with
        | [ expr ] -> expr
        | _ -> e_tuple l)
      <$> sep_by1 (pstr ",") term
    in
    let term = perbinop term [ ":=" ] in
    choice [ pelet pexpr; pematch pexpr; pefun pexpr; term ])
;;

let pdecl = ptoken (pdec_base pexpr)
let pdecl_delim = many (pstr ";;") *> pspace
let pprogram = pdecl_delim *> many (pdecl <* pdecl_delim)
let parse_program s = parse_string ~consume:Consume.All pprogram s

(* TESTS *)

let test_parser p eq pp code expected =
  match Angstrom.parse_string ~consume:Consume.All p code with
  | Ok ok ->
    (match eq ok expected with
     | true -> true
     | false ->
       Format.printf "Expected: %a\nActual: %a\n" pp expected pp ok;
       false)
  | Error err ->
    Format.printf "Error: %s\n" err;
    false
;;

let%test _ = test_parser parg equal_paramtyp pp_paramtyp "a" (ParSimple "a")
let%test _ = test_parser parg equal_paramtyp pp_paramtyp "~a" (ParLabeled "a")
let%test _ = test_parser parg equal_paramtyp pp_paramtyp "?a" (ParOptional "a")
let%test _ = test_parser pexpr equal_expr pp_expr "1" (EConst (CInt 1))
let%test _ = test_parser pexpr equal_expr pp_expr "+2" (EConst (CInt 2))
let%test _ = test_parser pexpr equal_expr pp_expr "10" (EConst (CInt 10))
let%test _ = test_parser pexpr equal_expr pp_expr "ab" (e_val "ab")
let%test _ = test_parser pexpr equal_expr pp_expr "-1" (e_un_op neg (e_const (c_int 1)))

let%test _ =
  test_parser pexpr equal_expr pp_expr {|"fgdeeee"|} (EConst (CString "fgdeeee"))
;;
