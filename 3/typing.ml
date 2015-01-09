open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t
type subst = (tyvar * ty) list

let rec subst_type sub t = match t with
    TyInt | TyBool -> t
  | TyFun (ty1, ty2) -> TyFun (subst_type sub ty1, subst_type sub ty2)
  | TyVar ty -> (match sub with
      [] -> t
    | (tyv, ty') :: subt ->
        if tyv = ty then subst_type subt ty'
        else subst_type subt t)

let eqs_of_subst s =
  List.map (fun (tvar, ty) -> (TyVar tvar, ty)) s

let subst_eqs s eqs =
  List.map (fun (t1, t2) -> (subst_type s t1, subst_type s t2)) eqs

(* (ty * ty) list -> subst *)
let rec unify l = match l with
    [] -> []
  | (ty1, ty2) :: t -> match ty1, ty2 with
      _, _ when ty1 = ty2 -> unify t
    | TyVar v, ty3
    | ty3, TyVar v ->
        if MySet.member v (freevar_ty ty3) then err ("Type Error: unify ty")
        else (v, ty3) :: (unify (subst_eqs [(v, ty3)] t))
    | TyFun (ty11, ty12), TyFun (ty21, ty22) ->
        unify ((ty11, ty21) :: (ty12, ty22) :: t)
    | _ -> err ("Type Error: unify")

let ty_prim op ty1 ty2 = match op with
    Plus | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | Land | Lor -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

let rec ty_exp tyenv = function
    Var x ->
    (try ([], Environment.lookup x tyenv) with
      Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty1, TyBool); (ty2, ty3)] in
      let s4 = unify eqs in (s4, subst_type s4 ty2)
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s, ty) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
      let eqs = (eqs_of_subst s) @ (eqs_of_subst s1) in
      let s2 = unify eqs in (s2, subst_type s2 ty)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty = ty_exp (Environment.extend id domty tyenv) exp in
      (s, TyFun (subst_type s domty, ranty))
  | AppExp (exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let domty = TyVar (fresh_tyvar ()) in
      let eqs = (ty1, TyFun (ty2, domty)) :: (eqs_of_subst s1) @ (eqs_of_subst s2) in
      let s3 = unify eqs in (s3, subst_type s3 domty)
  | LetRecExp (id, para, exp1, exp2) ->
      let ty1 = TyVar (fresh_tyvar ()) in
      let domty = TyFun (TyVar (fresh_tyvar ()), TyVar (fresh_tyvar ())) in
      let (s2, ty2) = ty_exp (Environment.extend para ty1 (Environment.extend id domty tyenv)) exp1 in
      let (s, ty) = ty_exp (Environment.extend id domty tyenv) exp2 in
      let eqs = (domty, TyFun (ty1, ty2)) :: (eqs_of_subst s2) @ (eqs_of_subst s) in
      let s3 = unify eqs in (s3, subst_type s3 ty)
  (*| _ -> err ("Not Implemented!")*)

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented!")
