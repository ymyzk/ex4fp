type nat = Zero | OneMoreThan of nat;;
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
type 'a seq = Cons of 'a * (unit -> 'a seq);;
type ('a, 'b) sum = Left of 'a | Right of 'b;;

(* Exercise 6.2 *)
let rec monus m n =
  match m, n with
  | Zero, _ -> Zero
  | _, Zero -> m
  | OneMoreThan m', OneMoreThan n' -> monus m' n'
;;

let zero  = Zero;;
let one   = OneMoreThan zero;;
let two   = OneMoreThan one;;
let three = OneMoreThan two;;

monus zero zero;;
(* nat = Zero *)
monus two zero;;
(* nat = OneMoreThan (OneMoreThan Zero) *)
monus three one;;
(* nat = OneMoreThan (OneMoreThan Zero) *)
monus one three;;
(* nat = Zero *)

(* Exercise 6.6 *)
let rec reflect = function
  | Lf           -> Lf
  | Br (x, l, r) -> Br (x, reflect r, reflect l)
;;

let comptree = Br(1, Br(2, Br(4, Lf, Lf),
                           Br(5, Lf, Lf)),
                     Br(3, Br(6, Lf, Lf),
                           Br(7, Lf, Lf)));;
reflect comptree;;
(* int tree = Br (1, Br (3, Br (7, Lf, Lf),
 *                          Br (6, Lf, Lf)),
 *                   Br (2, Br (5, Lf, Lf),
 *                          Br (4, Lf, Lf))) *)

(* preorder(reflect(t)) = reverse(postorder(t)) *)
(* inorder(reflect(t)) = reverse(inorder(t)) *)
(* postorder(reflect(t)) = reverse(preorder(t)) *)

(* Exercise 6.9 *)
let rec from n = Cons (n, fun () -> from (n + 1));;

let rec sift n (Cons (x, f)) =
  if (x mod n) = 0 then sift n (f ())
  else Cons (x, fun () -> sift n (f ()))
;;

let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f())));;

let primes = sieve (from 2);;

let rec nthseq n (Cons (x, f)) =
  if n = 1 then x
  else nthseq (n - 1) (f ());;

(*
nthseq (5569 + 3000) primes;;
*)
(* int = 88397 *)

(* Exercise 6.10.1 *)
let func_1 x y =
  match y with
  | Left z  -> Left (x, z)
  | Right z -> Right (x, z)
;;

(* Exercise 6.10.2 *)
let func_2 = function
  | Left z1,  Left z2  -> Left  (Left  (z1, z2))
  | Left z1,  Right z2 -> Right (Left  (z1, z2))
  | Right z1, Left z2  -> Right (Right (z1, z2))
  | Right z1, Right z2 -> Left  (Right (z1, z2))
;;

(* Exercise 6.10.3 *)
let func_3 (f1, f2) = function
  | Left x  -> f1 x
  | Right x -> f2 x
;;

(* Exercise 6.10.5 *)
let func_5 f a =
  match f with
  | Left g  -> Left (g a)
  | Right g -> Right (g a)
;;
