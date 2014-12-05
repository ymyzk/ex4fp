(* Exercise 7.2 *)
let incr i = i := !i + 1;;

let x = ref 3;;
incr x;;
(* unit = () *)
!x;;
(* int = 4 *)

(* Exercise 7.4 *)
let fact_imp n =
  let i = ref n and res = ref 1 in
    while (0 < !i) do
      res := !res * !i;
      i := !i - 1;
    done;
    !res
;;

fact_imp 0;;
(* int = 1 *)
fact_imp 1;;
(* int = 1 *)
fact_imp 2;;
(* int = 2 *)
fact_imp 3;;
(* int = 6 *)
fact_imp 4;;
(* int = 24 *)
fact_imp 5;;
(* int = 120 *)
fact_imp 10;;
(* int = 3628800 *)

(* Exercise 7.6 *)
(*
 * let x = ref [];; を実行すると
 * val x : '_a list ref = {contents = []}
 * が返される.
 * x は値多相のリストの ref になっている.
 *
 * 次に以下を実行すると,
 * (2 :: !x, true :: !x);;
 * Error: This expression has type int list
 *        but an expression was expected of type bool list
 *        Type int is not compatible with type bool
 * となる.
 * これは 2 :: !x を評価した段階で,
 * x の型が int list ref に定まるためで,
 * bool を int list ref に cons することは当然できないためエラーとなっている.
 *
 * 以上より, 値多相を利用することで OCaml は 7.1.4 のような
 * 事態の発生を防いでいる
 *)

(* Exercise 7.8 *)
let rec change = function
  | (_, 0) -> []
  | ((c :: rest) as coins, total) ->
      if total < c then
        change (rest, total)
      else
        (try
          c :: change (coins, total - c)
         with Failure "change" ->
           change (rest, total)) (* コインの種類を1つ減らして再挑戦 *)
   | _ -> raise (Failure "change") (* いまあるコインの種類で change できないとき *)
;;

change ([25; 10; 5; 1], 43);;
(* int list = [25; 10; 5; 1; 1; 1] *)
change ([5; 2], 16);;
(* int list = [5; 5; 2; 2; 2] *)
