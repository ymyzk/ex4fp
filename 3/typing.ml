open Syntax

exception TypeError of string

let err s = raise (TypeError s)

type tyenv = tysc Environment.t
type subst = (tyvar * ty) list

let rec subst_type sub t = match t with
    TyInt | TyBool -> t
  | TyFun (ty1, ty2) -> TyFun (subst_type sub ty1, subst_type sub ty2)
  | TyVar ty -> (match sub with
      [] -> t
    | (tyv, ty') :: subt ->
        if tyv = ty then subst_type subt ty'
        else subst_type subt t)

let freevar_tyenv tyenv =
  Environment.fold_right
    (fun tysc tyvars -> MySet.union (freevar_tysc tysc) tyvars) tyenv MySet.empty

let closure ty tyenv subst =
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv =
    MySet.bigunion
      (MySet.map (fun id -> freevar_ty (subst_type subst (TyVar id))) fv_tyenv') in
  let ids = MySet.diff (freevar_ty ty) fv_tyenv in
  TyScheme (MySet.to_list ids, ty)

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
        if MySet.member v (freevar_ty ty3) then err ("unify ty")
        else (v, ty3) :: (unify (subst_eqs [(v, ty3)] t))
    | TyFun (ty11, ty12), TyFun (ty21, ty22) ->
        unify ((ty11, ty21) :: (ty12, ty22) :: t)
    | _ -> err ("unify")

let ty_prim op ty1 ty2 = match op with
    Plus | Minus | Mult | Div -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Eq | Lt | Gt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | Land | Lor -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

let ty_prim_u op ty = match op with
  Minus -> ([(ty, TyInt)], TyInt)

let rec ty_exp tyenv = function
    Var x ->
    (try
      let TyScheme (vars, ty) = Environment.lookup x tyenv in
      let s = List.map (fun id -> (id, TyVar (fresh_tyvar ()))) vars in
      ([], subst_type s ty)
    with
      Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in (s3, subst_type s3 ty)
  | UnaryOp (op, exp) ->
      let (s1, ty1) = ty_exp tyenv exp in
      let (eqs2, ty) = ty_prim_u op ty1 in
      let eqs = (eqs_of_subst s1) @ eqs2 in
      let s2 = unify eqs in (s2, subst_type s2 ty)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty1, TyBool); (ty2, ty3)] in
      let s4 = unify eqs in (s4, subst_type s4 ty2)
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tys = closure ty1 tyenv s1 in
      let (s, ty) = ty_exp (Environment.extend id tys tyenv) exp2 in
      let eqs = (eqs_of_subst s) @ (eqs_of_subst s1) in
      let s2 = unify eqs in (s2, subst_type s2 ty)
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let s, ranty = ty_exp (Environment.extend id (tysc_of_ty domty) tyenv) exp in
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
      let tyenv1 = Environment.extend id (tysc_of_ty domty) tyenv in
      let tyenv1 = Environment.extend para (tysc_of_ty ty1) tyenv1 in
      let (s2, ty2) = ty_exp tyenv1 exp1 in
      let (s, ty) = ty_exp (Environment.extend id (tysc_of_ty domty) tyenv) exp2 in
      let eqs = (domty, TyFun (ty1, ty2)) :: (eqs_of_subst s2) @ (eqs_of_subst s) in
      let s3 = unify eqs in (s3, subst_type s3 ty)

let ty_let_decl tyenv exp =
  let (s, ty) = ty_exp tyenv exp in (s, subst_type s ty)

let ty_let_rec_decl tyenv id para exp =
  let ty1 = TyVar (fresh_tyvar ()) in
  let domty = TyVar (fresh_tyvar ()) in
  let tyenv = Environment.extend id (tysc_of_ty domty) tyenv in
  let tyenv = Environment.extend para (tysc_of_ty ty1) tyenv in
  let (s, ty) = ty_exp tyenv exp in
  (s, subst_type s ty)

let ty_decl tyenv = function
    Exp e ->
      let (s, ty) = ty_exp tyenv e in (tyenv, ty)
  | Decl (id, e) ->
      let (s, ty) = ty_let_decl tyenv e in
      (Environment.extend id (closure ty tyenv s) tyenv, ty)
  | RecDecl (id, para, exp) ->
      let (s, ty) = ty_let_rec_decl tyenv id para exp in
      (Environment.extend id (closure ty tyenv s) tyenv, ty)
