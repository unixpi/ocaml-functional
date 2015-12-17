(* ------------------------------------------------------------------------- *)
(* Author: Brigitte Pientka                                                  *)
(* COMP 302 Programming Languages - FALL 2015                                *)
(* Copyright © 2015 Brigitte Pientka                                        *)
(* ------------------------------------------------------------------------- *)
(*
  STUDENT NAME(S):
  STUDENT ID(S)  :


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
   val send_str : bin Stream.str -> bit Stream.str
   val rcv_str : bit Stream.str -> bin Stream.str
   val to_int  : bin Stream.str -> int Stream.str
       
 end 



(* Implement a module Bin that matches the signature BIN and provides 
   all the necessary functionality *)
module Bin : BIN =
  struct
    exception NotImp
    exception Error
    include Stream
    type bit = Zero | One | End
    type bin = int list

(**********************************************)
(*  Q2.1                                      *)
(**********************************************)

    (* Defining Binary Addition *)
    (* Similar to Integer Addition, where we use a carry. *) 
    (* Since this is binary addition, it can be 1 or 0 (bool) *)
    let incB (l: bin) = 
      let rec help (l:bin) = match l with
	| [] -> ([], true)
	| h::t -> let (t', carry) = help t in
		  if carry then match h with
				(* The carry is added to the head (If it is binary) *)
				| 0 -> (1::t', false) 
				| 1 -> (0::t', true)
				| _ -> raise Error
		  else (h::t', false)
      in
      let (m, carry) = help l in
      if carry then 1::m
      else m
		  
      
    let rec bin_str = {hd = [1]; tl = Susp (fun () -> map incB bin_str)}

			
(**********************************************)
(*  Q2.2                                      *)
(**********************************************)
			
    let intToBit (n : int) = match n with
      | 0 -> Zero
      | 1 -> One
      | _ -> raise Error
	
    let rec send_str s = match s.hd with
      | [] -> {hd = End; tl = Susp (fun () -> send_str (force s.tl))}      
      | h :: t -> let s = {hd = t; tl = Susp (fun () -> force s.tl)} in {hd = intToBit h; tl = Susp (fun () -> send_str s)}

									  
(**********************************************)
(*                  Q2.3                      *)
(**********************************************)

let rcv_str bits =
    let rec loop bits acc =
      match bits.Stream.hd with
      | Zero -> loop (Stream.force bits.Stream.tl) (0 :: acc)
      | One  -> loop (Stream.force bits.Stream.tl) (1 :: acc)
      | End  -> { Stream.hd = List.rev acc;
                  Stream.tl = Stream.Susp (fun () -> loop (Stream.force bits.Stream.tl) []) }
    in
    loop bits []
	 
(**********************************************)
(*                  Q2.4                      *)
(**********************************************)
    let rec bin_to_int b acc = match b with
      | [] -> acc
      | h::t -> match h with
		| 0 | 1 -> bin_to_int t (2*acc + h)
		| _ -> raise Error
	 
    let rec to_int s = {hd = (bin_to_int s.hd 0) ; tl = Susp (fun () -> to_int (force s.tl))}
end
