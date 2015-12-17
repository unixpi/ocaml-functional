(* HOMEWORK 1 : COMP 302 Fall 2015
   
   PLEASE NOTE:  

   * All code files must be submitted electronically
     BEFORE class on 24 Sep, 2015

  *  The submitted file name must be hw1.ml 

  *  Submitted solutions will be graded according to their correctness and elegance. 
     Please consult the OCaml style guide posted on the course website.

  *  Your program must type-check and run usig OCaml of at least OCaml 4.0

  * Remove all "raise NotImplemented" with your solutions
*)

exception NotImplemented
exception Error


(* ------------------------------------------------------------*)
(* QUESTION : Zipping and Unzipping                            *)
(* ------------------------------------------------------------*)

let rec zip l1 l2 = raise NotImplemented
 

let rec unzip l = raise NotImplemented


(*

PROVE THE FOLLOWING STATEMENT:

Theorem: unzip (zip l1 l2) = (l1, l2)

*)



(* ------------------------------------------------------------*)
(* QUESTION : Pocket Calculator                                *)
(* ------------------------------------------------------------*)

type instruction = Plus | Minus | Times | Div | Sin | Cos | Exp | Float of float

type stack = float list

let rec instr i s = raise NotImplemented

let rec prog instrs = raise NotImplemented


type exp = 
  | PLUS  of exp * exp  (* Plus *)
  | MINUS of exp * exp  (* Minus *)
  | TIMES of exp * exp  (* Times *)
  | DIV   of exp * exp  (* Div *)
  | SIN   of exp        (* Sin *)
  | COS   of exp        (* Cos *)
  | EXP   of exp * exp  (* Exp *)
  | FLOAT of float


let rec eval e = raise NotImplemented 


let rec to_instr e = raise NotImplemented 
