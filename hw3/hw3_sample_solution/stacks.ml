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
module type STACK =
  sig
    exception Error of string

    type 'a stack_obj = {top  : unit -> 'a ;
			 push : 'a -> unit ;
			 pop : unit -> unit}

    val new_stack : unit -> 'a stack_obj
  end

module Stack : STACK =
  struct

    type 'a stack_obj = {top  : unit -> 'a ;
			 push : 'a -> unit ;
			 pop  : unit -> unit}

    exception Error of string

    let new_stack () =
      let stack = ref [] in

      let top () =
        match !stack with
        | [] -> raise (Error "Stack is empty - No top element")
        | x::_ -> x
      in

      let push x =
        stack := x :: !stack
      in

      let pop () =
        match !stack with
        | [] -> raise (Error "Stack is empty - No top element")
        | x::rest -> stack := rest
      in

      { top; push; pop }

  end
