(* COMP 302 Assignment 2 *)

exception Error

(* -------------------------------------------------------------*)
(* QUESTION 1 :  Proofs  [25 points]                            *)
(* -------------------------------------------------------------*)

let rec concat l = match l with
  | [] -> ""
  | x::xs ->  x ^ (concat xs)

let concat' l =
let rec conc l acc = match l with
  | [] -> acc
  | x::xs -> conc xs (acc ^ x)
in
   conc l ""

(* Your friend implements the following version of concat;
   he claims it is a better function, as it is tail-recursive.

  A. Is he right? - Discuss the run-time behaviour between these two functions (5 points)

  B. Prove that given a list concat l produces the same result as conc l.
     (20 points)

*)

(* -------------------------------------------------------------*)
(* QUESTION 2 :  Warm-up [10 points]                            *)
(* -------------------------------------------------------------*)
(* remove_duplicates : ('a * 'a -> bool) -> 'a list -> 'a list *)

let rec remove_duplicates (eq : 'a * 'a -> bool) = function
  | [] -> []
  | x::xs -> x::List.filter (fun y -> not (eq (x, y))) (remove_duplicates eq xs)

(* -------------------------------------------------------------*)
(* QUESTION 3 :  Convolusion function  [15 points]               *)
(* -------------------------------------------------------------*)

(* conv(f, g, dx) = f'

  val conv: (real -> real) * (real -> real) -> real -> (real -> real)
 *)

let iter_sum f (lo, hi) inc =
  let rec sum' lo r =
      if (lo > hi) then r
    else
      sum' (inc lo) (r +. f lo)
  in
    sum' lo 0.0

let integral f (lo, hi) dx =
    dx *. iter_sum f ((lo +. (dx /. 2.0)), hi) (fun x -> (x +. dx))

let conv f g dx = fun x -> integral (fun y -> f y *. g (x +. y)) (0.0, x) dx
