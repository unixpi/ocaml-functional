(* Author: Brigitte Pientka
   Course: COMP 302, McGill University, Montreal, Canada
   Copyright © 2015 Brigitte Pientka

   Instructor generated course materials (e.g., handouts, notes, summaries,
   homeworks, exam questions, etc.) are protected by law and may not be copied
   or distributed in any form or in any medium without explicit permission of
   the instructor. Note that infringements of copyright can be subject to follow
   up by the University under the Code of Student Conduct and Disciplinary
   Procedures. 
*)
(* Parameterized stack module and signature 
   where we keep the elements stored in the stack
   and the concrete implementation of the stack
   abstract.
*)
module type STACK = 
  sig
    type stack
    type el
    val empty : unit -> stack
    val is_empty : stack -> bool
    val pop : stack -> stack option
    val push  : el -> stack -> stack
    val top : stack -> el option
    val size : stack -> int 
    val stack2list : stack -> el list
  end

module Stack   = 
  struct
    type el = int
    type stack = int list

    let empty () : stack = []

    let push i s= i::s

    let is_empty s = match s with
       | [] -> true
       | _::_ -> false

    let pop s = match s with 
       | [] -> None
       | _::t -> Some t

    let top s = match s with 
       | [] -> None
       | h::_ -> Some h
	 
	 
    let rec length s acc = match s with 
	| [] -> acc
	| x::t -> length t 1+acc

    let size s = length s 0

    let stack2list(s:stack) = s
  end

 module IntegerStack = (Stack : STACK with type el = int)

module IntStack :  (STACK with type el = int) = 
  struct
    type el = int
    type stack = int list

    let empty () : stack = []

    let push i s= i::s

    let is_empty s = match s with
       | [] -> true
       | _::_ -> false

    let pop s = match s with 
       | [] -> None
       | _::t -> Some t

    let top s = match s with 
       | [] -> None
       | h::_ -> Some h
	 
	 
    let rec length s acc = match s with 
	| [] -> acc
	| x::t -> length t 1+acc

    let size s = length s 0

    let stack2list(s:stack) = s
  end


module FloatStack : (STACK with type el = float) = 
  struct
    type el = float
    type stack = Empty | Put of el * stack

    let empty () : stack = Empty

    let push i s=  Put (i,s)

    let is_empty s =  match s with
       | Empty -> true
       | _  -> false

    let pop s = match s with 
       | Empty -> None
       | Put (_, t) -> Some t

    let top s =  match s with 
       | Empty -> None
       | Put (i, _ ) -> Some i
	 
	 
    let rec length s acc = match s with 
	| Empty -> acc
	| Put (_ , s) -> length s (1+acc)

    let size s = length s 0

    let rec stack2list(s:stack) = match s with 
      | Empty -> []
      | Put (x,s) -> x::stack2list s
  end
