(* Exercise 4.1 *)
let integral f a b =
  let n = 1000.0 in
  let delta = (b -. a) /. n in
  let rec area i r =
    if i <= 0.0 then
      r
    else
      (* tail recursion *)
      area (i -. 1.0) (r +. ((f (a +. (i -. 1.0) *. delta) +. f (a +. i *. delta)) *. delta) /. 2.0)
  in area n 0.0
;;

(* integrate sin x from 0 to pi = 2.0 *)
integral (fun x -> sin x) 0.0 3.14159265358979323846;;
(* float = 1.99999835506566193 *)

(* Exercise 4.4 *)
let curry f x y = f (x, y);;
let average (x, y) = (x +. y) /. 2.0;;
let curried_avg = curry average;;
average (4.0, 5.3);;
curried_avg 4.0, 5.3;;

let uncurry f (x, y) = f x y;;

let avg = uncurry curried_avg in
avg (4.0, 5.3);;
(* float = 4.65 *)

(* Exercise 4.5 *)
let rec repeat f n x =
  if 0 < n then
    repeat f (n - 1) (f x)
  else
    x
;;

let fib n =
  let (fibn, _) = repeat (fun (x, y) -> (x + y, x)) n (0, 1) in fibn
;;

fib 1;;
(* int = 1 *)
fib 2;;
(* int = 1 *)
fib 3;;
(* int = 2 *)
fib 4;;
(* int = 3 *)
fib 5;;
(* int = 5 *)
fib 50;;
(* int = 12586269025 *)

(* Exercise 4.7 *)
let k x y = x;;
(* val k : 'a -> 'b -> 'a = <fun> *)
let s x y z = x z (y z);;
(* val s : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c = <fun> *)

(*
 * s k k 1
 * s を定義に従って書き直すと以下のようになる
 * -> k 1 (k 1)
 * 次に先頭の k を定義に従って書き直すと以下のようになる
 * -> 1
 * よって s k k は恒等関数として働く
 *)

k (s k k) 1 2;;
(* int = 2 *)
