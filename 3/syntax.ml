(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Div | Eq | Lt | Gt | Land | Lor

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp

type program =
    Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp

type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty

let string_of_ty ty =
  let vars = ref [] in
  let string_of_tyvar tyvar =
    let rec index_of_tyvar pos = function
        [] -> vars := !vars @ [tyvar]; pos
      | x :: rest -> if x = tyvar then pos
                     else index_of_tyvar (pos + 1) rest in
  "'" ^ String.make 1 (char_of_int ((int_of_char 'a') + index_of_tyvar 0 !vars)) in
  let rec string_of_ty = function
    TyInt -> "int"
  | TyBool -> "bool"
  | TyVar v -> string_of_tyvar v
  | TyFun (ty1, ty2) ->
      let str1 = (match ty1 with
          TyFun (_, _) -> "(" ^ string_of_ty ty1 ^ ")"
        | _ -> string_of_ty ty1) in
      let str2 = string_of_ty ty2 in
      str1 ^ " -> " ^ str2
  in string_of_ty ty

let pp_ty ty =
  print_string (string_of_ty ty)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
      counter := v + 1; v
  in body

let rec freevar_ty = function
    TyVar var -> MySet.insert var MySet.empty
  | TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)
  | _ -> MySet.empty
