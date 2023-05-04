open Ast
open Base
open Interpret
module Format = Caml.Format

let ( = ) = Caml.( = )
let use_logging = false

let log fmt =
  if use_logging
  then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt
;;

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of typ * typ
  | `Pat_rebound of string
  | `Unexpected_non_let_expr of expr (* shouldn't happen *)
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_typ l pp_typ r
  | `Pat_rebound s ->
    Format.fprintf ppf "Variable %s is bound several times in this matching" s
  | `Unexpected_non_let_expr e -> Format.fprintf ppf "%a is not a let expr" pp_expr e
;;

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
   fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
 ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
   fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
 ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module VarSet = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar b -> b = v
    | TFun (_, _, l, r) -> occurs_in v l || occurs_in v r
    | TInt | TBool | TString | TList _ | TOpt _ | TTuple _ | TUninf | TRef _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TFun (_, _, l, r) -> helper (helper acc l) r
      | TInt | TBool | TString | TList _ | TOpt _ | TTuple _ | TUninf | TRef _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val pp : Caml.Format.formatter -> t -> unit
  val empty : t
  val singleton : fresh -> typ -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : fresh -> t -> typ

  val find : fresh -> t -> typ option
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax

  (* an association list. In real world replace it by a finite map *)
  type t = (fresh * typ) list

  let pp ppf subst =
    let open Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a" k pp_typ v))
      subst
  ;;

  let empty = []
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    match v with
    | TVar b when b = k -> return []
    | _ ->
      let* mapping = mapping k v in
      return [ mapping ]
  ;;

  let find_exn k xs = List.Assoc.find_exn xs k ~equal:Int.equal
  let find k xs = List.Assoc.find xs k ~equal:Int.equal
  let remove xs k = List.Assoc.remove xs k ~equal:Int.equal

  let apply s =
    let rec helper = function
      | TVar b as ty ->
        (match find_exn b s with
         | exception Not_found_s _ -> ty
         | x -> x)
      | TFun (ety, argtyp, l, r) -> TFun (ety, argtyp, helper l, helper r)
      | TList t -> TList (helper t)
      | TTuple ts -> TTuple (List.map ts ~f:helper)
      | (TInt | TBool | TString | TOpt _ | TUninf | TRef _) as other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TInt, TInt | TBool, TBool | TTuple [], TTuple [] -> return empty
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TFun (ety1, a1, l1, r1), TFun (ety2, a2, l2, r2) ->
      (match a1, a2 with
       | ASimple, ASimple ->
         let* subs1 = unify l1 l2 in
         let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
         compose subs1 subs2
       | (AOptional a1, AOptional a2 | ALabeled a1, ALabeled a2) when a1 = a2 ->
         let* subs1 = unify l1 l2 in
         let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
         compose subs1 subs2
       | _ ->
         let* tv = fresh >>| t_var in
         let* subs1 = unify r1 (TFun (ety2, a2, l2, tv)) in
         let* subs2 = unify (TFun (ety1, a1, l1, tv)) r2 in
         compose subs1 subs2)
    | TList l, TList r | TOpt l, TOpt r -> unify l r
    | TTuple (hd1 :: tl1), TTuple (hd2 :: tl2) ->
      let* subs1 = unify hd1 hd2 in
      let* subs2 = unify (TTuple tl1) (TTuple tl2) in
      compose subs1 subs2
    | _ -> fail (`Unification_failed (l, r))

  and extend s (k, v) =
    match List.Assoc.find s ~equal:Int.equal k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RList.fold_left s ~init:(return s2) ~f:(fun acc (k, v) ->
        let v = apply s2 v in
        let* mapping = mapping k v in
        return (mapping :: acc))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RList.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]
type scheme = S of binder_set * typ [@@deriving show { with_path = false }]

module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply sub (S (names, typ)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 typ)
  ;;

  let pp = pp_scheme
end

module TypeEnv = struct
  type t = (string * scheme) list

  let extend e h = h :: e
  let empty = []

  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, s) ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = List.Assoc.map env ~f:(Scheme.apply s)

  let pp ppf xs =
    Caml.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) -> Caml.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Caml.Format.fprintf ppf "|}%!"
  ;;

  let find_exn name xs = List.Assoc.find_exn ~equal:String.equal xs name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> TVar n

let instantiate : scheme -> typ R.t =
 fun (S (bs, t)) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
 fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env e xs =
  match List.Assoc.find_exn xs ~equal:String.equal e with
  | (exception Caml.Not_found) | (exception Not_found_s _) -> fail (`No_variable e)
  | scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let pp_env subst ppf env =
  let env : TypeEnv.t =
    List.map ~f:(fun (k, S (args, v)) -> k, S (args, Subst.apply subst v)) env
  in
  TypeEnv.pp ppf env
;;

let infer_ptrn =
  let rec helper env = function
    | PVal b when List.Assoc.mem env ~equal:String.equal b -> fail (`Pat_rebound b)
    | PVal b ->
      let* tv = fresh_var in
      return (TypeEnv.extend env (b, S (VarSet.empty, tv)), tv)
    | PConst (CInt _) -> return (env, TInt)
    | PConst (CBool _) -> return (env, TBool)
    | PConst (CString _) -> return (env, TString)
    | PConst CEmptyList ->
      let* tv = fresh_var in
      return (env, TList tv)
    | PTuple ps ->
      let* env, ts =
        RList.fold_left
          ps
          ~init:(return (env, []))
          ~f:(fun (env, ts) p ->
            let* env, t = helper env p in
            return (env, t :: ts))
      in
      return (env, TTuple ts)
    | PCons ([], p) ->
      let* env, t = helper env p in
      let* tv = fresh_var in
      let* s = unify (TList tv) t in
      return (TypeEnv.apply s env, Subst.apply s t)
    | PCons (hd :: tl, tl2) ->
      let* env, hdt = helper env hd in
      let* env, tlt = helper env (PCons (tl, tl2)) in
      let* s = unify (TList hdt) tlt in
      return (TypeEnv.apply s env, tlt)
    | PSome p -> helper env p >>| fun (env, t) -> env, t_opt t
    | PConst CNone -> fresh_var >>| fun v -> env, t_opt v
  in
  helper TypeEnv.empty
;;

let par_to_arg paramtyp =
  match paramtyp with
  | ParSimple _ -> ASimple
  | ParOptional s -> AOptional s
  | ParLabeled s -> ALabeled s
;;

let argname paramtyp =
  match paramtyp with
  | ParSimple s -> s
  | ParOptional s -> s
  | ParLabeled s -> s
;;

let rec has_simple_arg = function
  | TFun (_, ASimple, _, _) -> true
  | TFun (_, _, _, t2) -> has_simple_arg t2
  | _ -> false
;;

let rec all_args_erasable = function
  | TFun (Erasable, _, _, t2) -> all_args_erasable t2
  | TFun (_, _, _, _) -> false
  | _ -> true
;;

let rec pass_nones expr = function
  | TFun (Erasable, argtyp, _, t2) -> pass_nones (EApp (argtyp, expr, EConst CNone)) t2
  | _ -> expr
;;

let rec without_args t =
  match t with
  | TFun (_, _, _, t2) -> without_args t2
  | _ -> t
;;

let infer =
  let rec (helper : ?erase:bool -> TypeEnv.t -> expr -> (Subst.t * typ * expr) R.t) =
   fun ?(erase = true) env expr ->
    if erase
    then
      helper ~erase:false env expr
      >>| fun (s, t, e) ->
      ( s
      , (if all_args_erasable t then without_args t else t)
      , if all_args_erasable t then pass_nones e t else e )
    else (
      match expr with
      | EVal x -> lookup_env x env >>| fun (s, t) -> s, t, expr
      | EFun (argtyp, _, e1) ->
        let* tv =
          match argtyp with
          | ParOptional _ -> fresh_var >>| t_opt
          | _ -> fresh_var
        in
        let env2 = TypeEnv.extend env (argname argtyp, S (VarSet.empty, tv)) in
        let* s, ty, e1 = helper env2 e1 in
        let ety =
          match argtyp with
          | ParOptional _ when has_simple_arg ty -> Erasable
          | _ -> NonErasable
        in
        let trez = TFun (ety, par_to_arg argtyp, Subst.apply s tv, ty) in
        return (s, trez, EFun (argtyp, TUninf, e1))
      | EApp (argtyp, e1, e2) ->
        let* s1, t1, e1 = helper ~erase:false env e1 in
        let* s2, t2, e2 = helper (TypeEnv.apply s1 env) e2 in
        let* tv = fresh_var in
        let* s3 = unify (Subst.apply s2 t1) (TFun (NonErasable, argtyp, t2, tv)) in
        let trez = Subst.apply s3 tv in
        let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
        return (final_subst, trez, EApp (argtyp, e1, e2))
      | EConst c ->
        (match c with
         | CInt _ -> return TInt
         | CBool _ -> return TBool
         | CEmptyList -> fresh_var >>| fun v -> TList v
         | CString _ -> return TString
         | CNone -> fresh_var >>| fun v -> TOpt v)
        >>| fun t -> Subst.empty, t, expr
      | EMatch (scr, brnchs) ->
        let* s, scr_t, scr = helper env scr in
        let* t = fresh_var in
        RList.fold_left
          brnchs
          ~init:(return (s, t, []))
          ~f:(fun (s, t, bs) (p, e) ->
            let* p_env, p_t = infer_ptrn p in
            let env =
              List.fold_left p_env ~init:env ~f:(fun env (k, v) ->
                TypeEnv.extend env (k, v))
            in
            let* s2 = unify scr_t p_t in
            let* s3, e_t, e = helper env e in
            let* s4 = unify t e_t in
            let* final_subst = Subst.compose_all [ s; s2; s3; s4 ] in
            return (final_subst, Subst.apply final_subst t, (p, e) :: bs))
        >>| fun (s, t, bs) -> s, t, EMatch (scr, List.rev bs)
      | ELet ((Not_recursive, x, _, e1), e2) ->
        let* s1, t1, e1 = helper env e1 in
        let env2 = TypeEnv.apply s1 env in
        let t2 = generalize env2 t1 in
        let* s2, t3, e2 = helper (TypeEnv.extend env2 (x, t2)) e2 in
        let* final_subst = Subst.compose s1 s2 in
        return (final_subst, t3, ELet ((Not_recursive, x, TUninf, e1), e2))
      | ELet ((Recusive, x, _, e1), e2) ->
        let* tv = fresh_var in
        let env = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
        let* s1, t1, e1 = helper env e1 in
        let* s2 = unify (Subst.apply s1 tv) t1 in
        let* s = Subst.compose s2 s1 in
        let env = TypeEnv.apply s env in
        let t2 = generalize env (Subst.apply s tv) in
        let* s2, t2, e2 = helper TypeEnv.(extend (apply s env) (x, t2)) e2 in
        let* final_subst = Subst.compose s s2 in
        return (final_subst, t2, ELet ((Recusive, x, TUninf, e1), e2))
      | ETuple es ->
        let* subs, ts, es =
          RList.fold_left
            es
            ~init:(return (Subst.empty, [], []))
            ~f:(fun (subs, ts, es) e ->
              let* subs2, t, e = helper env e in
              let* final_subst = Subst.compose subs subs2 in
              return (final_subst, t :: ts, e :: es))
        in
        return (subs, TTuple (List.rev ts), ETuple (List.rev es))
      | EBinop (e1, bin_op, e2) ->
        let* s1, t1, e1 = helper env e1 in
        let* s2, t2, e2 = helper env e2 in
        let t1 = Subst.apply s2 t1 in
        let t2 = Subst.apply s1 t2 in
        (match bin_op with
         | Add | Mul | Sub | Div ->
           let* s3 = unify TInt t1 in
           let* s4 = unify TInt (Subst.apply s3 t2) in
           let* s = Subst.compose_all [ s1; s2; s3; s4 ] in
           return (s, TInt)
         | Les | Gre | Eq | Leseq | Greeq | Neq ->
           let* s3 = unify t1 t2 in
           let* s = Subst.compose_all [ s1; s2; s3 ] in
           return (s, TBool)
         | Con ->
           let* s3 = unify (t_list t1) t2 in
           let* s = Subst.compose_all [ s1; s2; s3 ] in
           return (s, Subst.apply s t2)
         | And | Or ->
           let* s3 = unify TBool t1 in
           let* s4 = unify TBool (Subst.apply s3 t2) in
           let* s = Subst.compose_all [ s1; s2; s3; s4 ] in
           return (s, TBool)
         | _ -> fail (`Unification_failed (t1, t2)))
        >>| fun (s, t) -> s, t, EBinop (e1, bin_op, e2)
      | EUnop (un_op, e1) ->
        let* s1, t1, e1 = helper env e1 in
        (match un_op with
         | Neg ->
           let* s2 = unify TInt t1 in
           let* s = Subst.compose s1 s2 in
           return (s, TInt, EUnop (un_op, e1)))
      | ESome e ->
        let* s, t, e = helper env e in
        return (s, t_opt t, ESome e)
      | EParens e -> helper env e >>| fun (s, t, e) -> s, t, EParens e)
  in
  helper
;;

let infer_program p =
  run
    (let* _, p =
       RList.fold_left
         p
         ~init:(return ([], []))
         ~f:(fun (env, acc) ((_, name, _, _) as decl) ->
           let* _, t, e = infer env (ELet (decl, EVal name)) in
           match e with
           | ELet ((recty, name, _, expr), _) ->
             return
               (TypeEnv.extend env (name, generalize env t), (recty, name, t, expr) :: acc)
           | e -> fail (`Unexpected_non_let_expr e))
     in
     return (List.rev p))
;;
