(* ---------------------------------------------------------------------------- *)
(* Author: Brigitte Pientka                                                     *)
(* COMP 302 Programming Languages - FALL 2015                                   *)
(* Copyright © 2012 Brigitte Pientka                                           *)
(* ---------------------------------------------------------------------------- *)

module type ORDERED =
  sig
    type t  (* making the type abstract *)
    type comparison = Less | Equal | Greater

    (* elements seperated by a space in the input string *)
    val explode : string -> t list
    val implode : t list -> string

    val compare : t -> t -> comparison
    val lt : t * t -> bool
    val eq : t * t -> bool
  end

(* The signature above will be augmented with
   the specific specialization for type t *)

module  CharLt : ORDERED =
  struct
    type t = char
    type comparison = Less | Equal | Greater

    let rec tabulate f n = 
      let rec tab n acc = 
	if n = 0 then (f 0)::acc
	else tab (n-1) ((f n)::acc)
      in
	tab n []

   let explode s = 
     tabulate (fun n -> String.get s n) ((String.length s) - 1)
       
   (* implode : char list -> string *)
   let implode l = 
     List.fold_right (fun c s -> Char.escaped c ^ s) l ""

    let lt (s,t) = s < t
    let eq (s,t) = (s = t)

    let compare s t = 
      if eq (s,t) then Equal 
      else if lt (s,t) then Less
      else Greater
  end

module StringLt : ORDERED =
  struct
    type t = string
    type comparison = Less | Equal | Greater


    let explode s = Str.split (Str.regexp " ") s
    let implode s =
      let p = List.fold_left (fun s b -> s ^ " " ^ b) "" s in 
      String.sub p 1 (String.length p)

    let lt(s,t) = String.length(s) < String.length(t)
    let eq(s,t) = String.length(s) = String.length(t)

    let compare s t = 
      if eq (s,t) then Equal 
      else if lt (s,t) then Less
      else Greater
  end


module IntLt : ORDERED = 
  struct
    type t = int
    type comparison = Less | Equal | Greater

    let explode s = 
      List.map (fun n -> int_of_string n)
	(Str.split (Str.regexp " ") s)

    let implode s = 
      let p = List.fold_left (fun s b ->  s ^ " " ^ (string_of_int b)) "" s in 
      String.sub p 1 (String.length p)

    let lt(s,t) = s < t
    let eq(s,t) = (s = t)

    let compare s t = 
      if eq (s,t) then Equal 
      else if lt (s,t) then Less
      else Greater
  end

