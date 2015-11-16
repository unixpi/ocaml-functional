(* ------------------------------------------------------------------------- *)
(* Author: Brigitte Pientka                                                  *)
(* COMP 302 Programming Languages - FALL 2015                                *)
(* Copyright © 2015 Brigitte Pientka                                        *)
(* ------------------------------------------------------------------------- *)
(*
  STUDENT NAME(S): Sacha Saint-Leger
  STUDENT ID(S)  : 260473392


  Fill out the template below.

*)
module type STREAM = 
  sig
    type 'a susp = Susp of (unit -> 'a)
    type 'a str = {hd: 'a  ; tl : ('a str) susp} 

    val force: 'a susp -> 'a
    val map  : ('a -> 'b) -> 'a str -> 'b str 
    val take : int -> 'a str -> 'a list
  end 


module Stream : STREAM = 
  struct
    (* Suspended computation *)
    type 'a susp = Susp of (unit -> 'a)

    (* force: *)
    let force (Susp f) = f ()

    type 'a str = {hd: 'a  ; tl : ('a str) susp} 

    (* map: ('a -> 'b) -> 'a str -> 'b str *)
    let rec map f s = 
      { hd = f (s.hd) ; 
	tl = Susp (fun () -> map f (force s.tl))
}

    (* Inspect a stream up to n elements *)
    let rec take n s = match n with 
      | 0 -> []
      | n -> s.hd :: take (n-1) (force s.tl)
	  
  end 



module type BIN = 
 sig
   type bit = Zero | One | End
   type bin = int list

   val bin_str : bin Stream.str 
(*   val send_str : bin Stream.str -> bit Stream.str
   val rcv_str : bit Stream.str -> bin Stream.str
   val to_int  : bin Stream.str -> int Stream.str
 *)
 end 

(* Implement a module Bin that matches the signature BIN and provides 
   all the necessary functionality *)
module Bin : BIN =
  struct

   type bit = Zero | One | End
   type bin = int list

   let rec rev l = match l with
     | [] -> []
     | h::t -> (rev t) @ [h]
	
   let rec add_one_binary a =
     match a with
     | [] -> []
     | 0::t -> 1::t
     | 1::[] -> 0::1::(add_one_binary [])
     | 1::t -> 0::(add_one_binary t)

   let add_one a = rev (add_one_binary (rev  a))

   let rec bin_str_from b =
     { Stream.hd = b ;							                               Stream.tl = Susp (fun () -> bin_str_from (add_one b))
     }
		
   let bin_str = bin_str_from [1]
			      
    		     
     
  end
    
