open Syntax

exception Error of string

let err s = raise (Error s)

(* Type Environment *)
type tyenv = ty Environment.t
type subst = (tyvar * ty) list

let ty_prim op ty1 ty2 = match op with
    Plus -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
              | _ -> err ("Argument must be of integer: +"))
  | Mult -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
              | _ -> err ("Argument must be of integer: *"))
  | Lt -> (match ty1, ty2 with
                TyInt, TyInt -> TyBool
              | _ -> err ("Argument must be of integer: <"))
  | Land -> (match ty1, ty2 with
                TyBool, TyBool -> TyBool
              | _ -> err ("Argument must be of boolean: &&"))
  | Lor -> (match ty1, ty2 with
                TyBool, TyBool -> TyBool
              | _ -> err ("Argument must be of boolean: ||"))
  | _ -> err ("Not Implemented!")

let rec ty_exp tyenv = function
    Var x ->
    (try Environment.lookup x tyenv with
      Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
        ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
      let tyarg1 = ty_exp tyenv exp1 in
      (match tyarg1 with
        | TyBool -> (let tyarg2 = ty_exp tyenv exp2 in
                     let tyarg3 = ty_exp tyenv exp3 in
                     match tyarg2, tyarg3 with
                        TyInt, TyInt -> TyInt
                      | TyBool, TyBool -> TyBool
                      | _ -> err ("Type error: if"))
        | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      ty_exp (Environment.extend id tyarg1 tyenv) exp2
  | _ -> err ("Not Implemented!")

let ty_decl tyenv = function
    Exp e -> ty_exp tyenv e
  | _ -> err ("Not Implemented!")

let rec subst_type sub ty0 = match ty0 with
    TyInt -> TyInt
  | TyBool -> TyBool
  | TyFun (ty1, ty2) -> TyFun (subst_type sub ty1, subst_type sub ty2)
  | TyVar v -> (match sub with
      [] -> err ("Error")
    | s :: subt -> (match s with
        tyv, t -> if tyv = v then subst_type sub t else subst_type subt ty0))

let rec unify l = match l with
    [] -> []
  | (ty1, ty2) :: t ->
      if ty1 = ty2 then
        unify t
      else
        (match ty1, ty2 with
           TyVar v, _ ->
             if MySet.member v (freevar_ty ty2) then
               err ("Error")
             else
               (ty1, ty2) :: unify (List.map (fun (t1, t2) -> (subst_type [(v, ty2)] t1, subst_type [(v, ty2)] t2)) l)
         | _, TyVar v ->
             if MySet.member v (freevar_ty ty1) then
               err ("Error")
             else
               (ty2, ty1) :: unify (List.map (fun (t1, t2) -> (subst_type [(v, ty1)] t1, subst_type [(v, ty1)] t2)) l)
         | TyFun (t11, t12), TyFun (t21, t22) -> unify ((t11, t21) :: (t12, t22) :: t)
         | _ -> err ("Error"))
