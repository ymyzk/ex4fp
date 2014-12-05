(* Exercise 3.7.1 *)
let rec pow (x, n) =
  if n = 0 then
    1.0
  else
    x *. pow (x, n - 1)
;;

pow (2.0, 0);;
(* int = 1 *)
pow (2.0, 8);;
(* int = 256 *)
pow (2.0, 16);;
(* int = 65536 *)

(* Exercise 3.7.2 *)
let rec pow (x, n) =
  if n = 0 then
    1.0
  else if n mod 2 = 0 then
    pow (x *. x, n / 2)
  else
    (pow (x *. x, n / 2)) *. x
;;

pow (2.0, 0);;
(* int = 1 *)
pow (2.0, 8);;
(* int = 256 *)
pow (2.0, 16);;
(* int = 65536 *)

(* Exercise 3.8.1 *)
let rec powi (x, n, r) =
  if n = 0 then
    r
  else
    powi (x, n - 1, x *. r)
;;
let powi (x, n) = powi (x, n, 1.0);;

powi (2.0, 0);;
(* int = 1 *)
powi (2.0, 8);;
(* int = 256 *)
powi (2.0, 16);;
(* int = 65536 *)

(* Exercise 3.8.2 *)
let rec powi (x, n, r) =
  if n = 0 then
    r
  else if n mod 2 = 0 then
    powi (x *. x, n / 2, r)
  else
    powi (x *. x, n / 2, r *. x)
;;
let powi (x, n) = powi (x, n, 1.0);;

powi (2.0, 0);;
(* int = 1 *)
powi (2.0, 8);;
(* int = 256 *)
powi (2.0, 16);;
(* int = 65536 *)

(* Exercise 3.11.1 *)
let rec gcd n m =
  if n = 0 then
    m
  else
    gcd (m mod n) n
;;

gcd 3 5;;
(* int = 1 *)
gcd 1071 1029;;
(* int = 21 *)

(* Exercise 3.11.2 *)
let rec comb (n, m) =
  if m = 0 || n = m then
    1
  else
    comb (n - 1, m) + comb (n - 1, m - 1)
;;

comb (4, 3);;
(* int = 4 *)
comb (4, 2);;
(* int = 6 *)
comb (3, 3);;
(* int = 1 *)

(* Exercise 3.11.3 *)
let rec fib_iter (n, a, b) =
  if n = 0 then
    1
  else if n = 1 then
    a
  else
    fib_iter (n - 1, a + b, a)
;;
let fib_iter n = fib_iter (n, 1, 0);;

fib_iter 1;;
(* int = 1 *)
fib_iter 2;;
(* int = 1 *)
fib_iter 3;;
(* int = 2 *)
fib_iter 4;;
(* int = 3 *)
fib_iter 5;;
(* int = 5 *)
fib_iter 50;;
(* int = 12586269025 *)

(* Exercise 3.11.4 *)
let rec max_ascii (str, i, n, r) =
  if i = n then
    r
  else
    if r < str.[i] then
      max_ascii (str, i + 1, n, str.[i])
    else
      max_ascii (str, i + 1, n, r)
;;

let max_ascii str = max_ascii (str, 0, String.length(str), '\000');;

max_ascii "ABC";;
(* char = 'C' *)
max_ascii "ABCdefGHI";;
(* char = 'f' *)
