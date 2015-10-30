

(* ------------------------------------------------------------------------- *)
(* Author: Brigitte Pientka                                                  *)
(* COMP 302 Programming Languages - FALL 2015                                *)
(* Copyright © 2015 Brigitte Pientka                                        *)
(* 

------------------------------------------------------------------------- *)
(*
  STUDENT NAME(S): Sacha Saint-Leger
  STUDENT ID(S)  : 260473392


  Fill out the template below.

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

  (* write a function path which accepts an expression e and a p : exp -> bool and 
     returns the path to the first sub expression in e that satisfies p when traversing
     the AST of e from left to right. Use the excpetion No_Path to backtrack
     through the tree  *)
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

    let rec path e p = match e with
    | Int(a) -> if p (Int(a)) then H else raise No_Path
    | Plus (a, b) -> if p (Plus (a, b)) then H
 		   else
		     (try L (path a p) with No_Path -> R (path b p))
    | Minus (a, b) -> if p (Minus (a, b)) then H
 		   else
		     (try L (path a p) with No_Path -> R (path b p))
    | Times (a, b) -> if p (Times (a, b)) then H
 		   else
		     (try L (path a p) with No_Path -> R (path b p))
    | Div (a, b) -> if p (Div (a, b)) then H
 		   else
		     (try L (path a p) with No_Path -> R (path b p))
    | Exp (a, b) -> if p (Exp (a, b)) then H
 		   else
		     try L (path a p) with No_Path -> R (path b p)
;; 
  			      
  						
    
let rec all_paths e p = match e with
    | Int(a) -> (path (Int(a)) p) :: []
    | Plus (a, b) -> ((path (Plus (a,b)) p) :: (L (path a p)) :: (R (path b p)) :: [])
	 ;;
  let div_by_zero e = raise (Error "Not Implemented Yet")

 end;;

 module Test = 
 struct
   open ArithExp
   let p1 expression =
     expression = Int(3);;
   let p2 expression =
     expression = Times(Int(10), Int(5));;
   let e1 = Plus(Times(Int(3), Int(2)), Div(Times(Int(10), Int(5)), Int(0)))
  let e2 = Plus(Times(Int(3), Int(2)), Div(Times(Int(10), Int(5)), Times(Int(3), Int(2))))
;;
   path e1 p1;; (* should return L(L(H))) *)		
   path e1 p2;; (* should return R(L(H)) *)
 end 
