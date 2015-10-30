(* ------------------------------------------------------------------------ *)
(* Author: Brigitte Pientka                                                 *)
(* COMP 302 Programming Languages - FALL 2015                               *)
(* Copyright © 2015 Brigitte Pientka                                        *)
(* ------------------------------------------------------------------------ *)
(*
  STUDENT NAME(S):
  STUDENT ID(S)  :


  Fill out the template below.

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
			 pop : unit -> unit}

    exception Error of string 

    let new_stack () = raise (Error "Not Implemented Yet.")

  end


