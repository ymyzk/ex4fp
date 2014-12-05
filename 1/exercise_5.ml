(* Exercise 5.3.1 *)
let rec downto0 n =
  if n < 0 then
    []
  else
    n :: (downto0 (n - 1))
;;

downto0 0;;
(* int list = [0] *)
downto0 1;;
(* int list = [1; 0] *)
downto0 10;;
(* int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0] *)

(* Exercise 5.3.3 *)
let rec concat l =
  match l with [] -> []
  | h :: t -> h @ concat t
;;

concat [[0; 3; 4]; [2]; [5; 0]; []];;
(* int list = [0; 3; 4; 2; 5; 0] *)

(* Exercise 5.3.4 *)
let rec zip a b =
  match a, b with
  | [], _ -> []
  | _, [] -> []
  | (ha :: ta), (hb :: tb) -> (ha, hb) :: (zip ta tb)
;;

zip [] [];;
(* ('a * 'b) list = [] *)
zip [1; 2; 3] [];;
(* (int * 'a) list = [] *)
zip [] [1; 2; 3];;
(* ('a * int) list = [] *)
zip [1; 2; 3] [11; 12];;
(* (int * int) list = [(1, 11); (2, 12)] *)
zip [11; 12] [1; 2; 3];;
(* (int * int) list = [(11, 1); (12, 2)] *)
zip [1; 2; 3] [11; 12; 13];;
(* (int * int) list = [(1, 11); (2, 12); (3, 13)] *)

(* Exercise 5.3.5 *)
let rec filter f l =
  match l with
  | []     -> []
  | [i]    -> if f i then [i] else []
  | h :: t -> if f h then h :: filter f t else filter f t
;;

filter (fun x -> 0 < x) [-9; 0; 2; 5; -3];;
(* int list = [2; 5] *)

(* Exercise 5.3.6.a *)
let rec belong a s =
  match s with
  | []     -> false
  | [i]    -> a = i
  | h :: t -> if a = h then true else belong a t
;;

belong 3 [4; 5; -2; 3; 1];;
(* bool = true *)
belong 0 [4; 5; -2; 3; 1];;
(* bool = false *)

(* Exercise 5.3.6.b *)
let rec intersect s1 s2 =
  match s1 with
  | []       -> []
  | [i]      -> if belong i s2 then [i] else []
  | h1 :: t1 -> if belong h1 s2 then h1 :: intersect t1 s2 else intersect t1 s2
;;

intersect [] [];;
(* 'a list = [] *)
intersect [3; 5] [];;
(* int list = [] *)
intersect [] [4; 5; -2; 3; 1];;
(* int list = [] *)
intersect [3; 5] [4; 5; -2; 3; 1];;
(* int list = [3; 5] *)
intersect [1; 7; 8; 4; 3] [4; 5; -2; 3; 1];;
(* int list = [1; 4; 3] *)
intersect [10; -8] [4; 5; -2; 3; 1];;
(* int list = [] *)

(* Exercise 5.3.6.c *)
let rec union s1 s2 =
  match s1 with
  | []     -> s2
  | [i]    -> if belong i s2 then s2 else i :: s2
  | h :: t -> if belong h s2 then union t s2 else h :: union t s2
;;

union [] [];;
(* 'a list = [] *)
union [3; 5] [];;
(* int list = [3; 5] *)
union [] [3; 5];;
(* int list = [3; 5] *)
union [1; 3; 5; 7] [3; 5; 7; 9];;
(* int list = [1; 3; 5; 7; 9] *)

(* Exercise 5.3.6.d *)
let rec diff s1 s2 =
  match s1 with
  | []     -> []
  | [i]    -> if belong i s2 then [] else [i]
  | h :: t -> if belong h s2 then diff t s2 else h :: diff t s2
;;

diff [] [];;
(* 'a list = [] *)
diff [3; 5] [];;
(* int list = [3; 5] *)
diff [] [3; 5];;
(*  int list = [] *)
diff [1; 3; 5; 7] [3; 5; 7; 9];;
(*  int list = [1] *)

(* Exercise 5.6 *)
let rec quicker l sorted =
  match l with
  | [] -> sorted
  | [x] -> x :: sorted
  | x :: xs ->
      let rec partition left right = function
        | [] -> quicker left (x :: quicker right sorted)
        | y :: ys ->
            if x < y then
              partition left (y :: right) ys
            else
              partition (y :: left) right ys
      in partition [] [] xs
;;

quicker [] [];;
(* 'a list = [] *)
quicker [3] [];;
(* int list = [3] *)
quicker [3; 4; 1] [];;
(* int list = [1; 3; 4] *)
quicker [3; 4; 1; -3; 5] [];;
(* int list = [-3; 1; 3; 4; 5] *)
