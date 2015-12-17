(* ---------------------------------------------------------------------------- *)
(* Author: Brigitte Pientka                                                     *)
(* COMP 302 Programming Languages - FALL 2015                                   *)
(* Copyright © 2012 Brigitte Pientka                                           *)
(* ---------------------------------------------------------------------------- *)
(*
  STUDENT NAME(S):
  STUDENT ID(S)  :


  Fill out the templage below.

*)


module type EXP =
sig

  type exp =
    | Plus  of exp * exp
    | Minus of exp * exp
    | Times of exp * exp
    | Div   of exp * exp
    | Exp   of exp * exp
    | Int   of int

  type path = L of path | R of path | H

  exception No_Path
  exception Error of string

  val eval : exp -> int
  val path : exp -> (exp -> bool) -> path
  val all_paths: exp -> (exp -> bool) -> path list
  val div_by_zero: exp -> path option

end


module ArithExp : EXP =
struct
  type exp =
    | Plus  of exp * exp
    | Minus of exp * exp
    | Times of exp * exp
    | Div   of exp * exp
    | Exp   of exp * exp
    | Int   of int


  type path = L of path | R of path | H

  exception No_Path
  exception Error of string


  let rec eval e = match e with
    | Int n -> n
    | Plus(e1, e2) -> eval e1 + eval e2
    | Minus(e1, e2) -> eval e1 - eval e2
    | Times(e1, e2) -> eval e1 * eval e2
    | Div(e1, e2) -> eval e1 / eval e2
    | Exp(e1, e2) -> truncate ((float (eval e1)) ** (float (eval e2)))

  let rec path e p =
    if p e then
      H
    else
      match e with
      | Int _ -> raise No_Path
      | Plus(e1, e2)
      | Minus(e1, e2)
      | Times(e1, e2)
      | Div(e1, e2)
      | Exp(e1, e2) ->
         (try
            L (path e1 p)
          with No_Path ->
            R (path e2 p))


  let rec all_paths e p =
    match e with
    | Int _ -> if p e then [H] else []
    | Plus(e1, e2)
    | Minus(e1, e2)
    | Times(e1, e2)
    | Div(e1, e2)
    | Exp(e1, e2) ->
       let self = if p e then [H] else [] in
       let left_paths = List.map (fun x -> L(x)) (all_paths e1 p) in
       let right_paths = List.map (fun x -> R(x)) (all_paths e2 p) in
       self @ left_paths @ right_paths

  let div_by_zero e =
    let is_div_by_zero e =
      match e with
      | Div(_, right) -> (try eval right = 0 with Division_by_zero -> false)
      | _ -> false
    in

    try
      Some (path e is_div_by_zero)
    with No_Path ->
      None


end;;

module Test =
struct
  open ArithExp

  let e1 = Plus(Times(Int(3), Int(2)), Div(Times(Int(10), Int(5)), Int(0)))
  let e2 = Plus(Times(Int(3), Int(2)), Div(Times(Int(10), Int(5)), Times(Int(3), Int(2))))
  let e3 = Plus(Times(Int(3), Int(2)), Div(Times(Int(10), Int(5)), Minus(Int(3), Plus(Int(1), Int(2)))))


end
