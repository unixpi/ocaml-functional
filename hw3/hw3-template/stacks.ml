(* ------------------------------------------------------------------------ *)
(* Author: Brigitte Pientka                                                 *)
(* COMP 302 Programming Languages - FALL 2015                               *)
(* Copyright © 2015 Brigitte Pientka                                        *)
(* ------------------------------------------------------------------------ *)
(*
  STUDENT NAME(S): Sacha Saint-Leger
  STUDENT ID(S)  : 260473392


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

    let new_stack () =
      let stack = ref [] in
      { top = (fun () -> if !stack = [] then raise (Error "no top element")
			 else
			   match !stack with
			     h :: t -> h) ;
	push = (fun x -> stack := (x :: !stack); () );
	pop = (fun () -> stack := (if !stack = [] then raise (Error "no top element")
				   else
				     match !stack with
				       h :: t -> t)); }
  			       
				       
	   
  end


