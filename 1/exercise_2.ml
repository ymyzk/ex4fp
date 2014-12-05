(* Exercise 2.1.1 *)
float_of_int 3 +. 2.5;;
(* float = 5.5 *)

(* Exercise 2.1.2 *)
int_of_float 0.7;;
(* int = 0 *)

(* Exercise 2.1.3 *)
if "11" > "100" then "foo" else "bar";;
(* string = "foo" *)

(* Exercise 2.1.4 *)
char_of_int ((int_of_char 'A') + 20);;
(* char = 'U' *)
(* 文字コード上の演算 *)

(* Exercise 2.1.5 *)
int_of_string "0xff";;
(* int = 255 *)

(* Exercise 2.1.6. *)
5.0 ** 2.0;;
(* float = 25. *)

(* Exercise 2.6.2 *)
let jpy_to_usd jpy =
  let usc = ((float_of_int jpy) *. 100.0 /. 111.12) in
    if 0.5 <= usc -. (floor usc) then
      floor(usc +. 1.0) /. 100.0
    else
      floor(usc) /.100.0
;;
jpy_to_usd 110;;
(* float = 0.99 *)
(* 切り上げ *)
jpy_to_usd 111;;
(* float = 1. *)
(* 切り上げ *)
jpy_to_usd 112;;
(* float = 1.01 *)
(* 切り上げ *)
jpy_to_usd 113;;
(* float = 1.02 *)
(* 切り上げ *)
jpy_to_usd 114;;
(* float = 1.03 *)
(* 切り上げ *)
jpy_to_usd 115;;
(* float = 1.03 *)
(* 切り下げ *)

(* Exercise 2.6.4 *)
let capitalize c =
  let code = int_of_char c in
    if 97 <= code && code <= 122 then
      char_of_int (code - 32)
    else
      c
;;

capitalize 'A';;
(* char = 'A' *)
capitalize 'a';;
(* char = 'A' *)
capitalize '1';;
(* char = '1' *)
