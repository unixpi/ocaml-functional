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
(* Defining a signature *)
module type ORDERED =
  sig
    type t  (* making the type abstract *)
    type comparison = Less | Equal | Greater

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

    let lt (s,t) = s < t
    let eq(s,t) = (s = t)

    let compare s t = 
      if eq (s,t) then Equal 
      else if lt (s,t) then Less
      else Greater
  end

module StringLt =
  struct
    type t = string
    type comparison = Less | Equal | Greater

    let lt(s,t) = String.length(s) < String.length(t)
    let eq(s,t) = String.length(s) = String.length(t)

    let compare s t = 
      if eq (s,t) then Equal 
      else if lt (s,t) then Less
      else Greater
  end

module IntLt = 
  struct
    type t = int
    type comparison = Less | Equal | Greater

    let lt(s,t) = s < t
    let eq(s,t) = (s = t)

    let compare s t = 
      if eq (s,t) then Equal 
      else if lt (s,t) then Less
      else Greater
  end


module IntMod50 =
  struct
    type t = int
    type comparison = Less | Equal | Greater

    let lt(m,n) = (m mod 50) < (n mod 50) 
    let eq(m,n) = (m mod 50) = (n mod 50) 
    let compare s t = 
      if eq (s,t) then Equal 
      else if lt (s,t) then Less
      else Greater

  end

(* Defining set module which is parameterized by the elements
   and comparing elements *)
module Set (Elt: ORDERED) = 
  struct
    type element = Elt.t

    type set = element list

    let empty = []

    let rec add x s = match s with
      | [] -> [x]
      | hd::tl -> match Elt.compare x hd with
          | Elt.Equal   -> s         (* x is already in s *)
          | Elt.Less    -> x :: s    (* x is smaller than all elements of s *)
          | Elt.Greater -> hd :: add x tl

    let rec member x s = match s with
      | [] -> false
      | hd::tl -> match Elt.compare x hd with
          | Elt.Equal   -> true     (* x belongs to s *)
          | Elt.Less    -> false    (* x is smaller than all elements of s *)
          | Elt.Greater -> member x tl
      end;;

(* Useful for code documentation *)
module StringSet = Set(StringLt)
module IntSet = Set(IntLt)
module IntMod50Set = Set(IntMod50)

(* Using module signatures to hide what information gets exposed. *)
module type ABSTRACT_SET = functor (Elt: ORDERED) -> 
  sig
    type element = Elt.t      (* concrete *)
    type set                  (* abstract *)
    val empty : set
    val add : element -> set -> set
    val member : element -> set -> bool
  end;;


module AbstractSet  = (Set : ABSTRACT_SET)
module AbstractStringSet = AbstractSet(StringLt)
module AbstractIntSet = AbstractSet(IntLt)
module AbstractIntMod50Set = AbstractSet(IntMod50)


(* More elegantly 

  We define a parameterized module AbstractSet2 directly and
  declare its "return" type, i.e. the kind of module it 
  defines.

*)
module type SET =
    sig
      type element
      type set
      val empty : set
      val add : element -> set -> set
      val member : element -> set -> bool
    end;;

module AbstractSet2 (Elt: ORDERED) : (SET with type element = Elt.t) =
struct
  type element = Elt.t

  type set = element list
      
  let empty = []
      
  let rec add x s = match s with
    | [] -> [x]
    | hd::tl -> match Elt.compare x hd with
          Elt.Equal   -> s         (* x is already in s *)
        | Elt.Less    -> x :: s    (* x is smaller than all elements of s *)
        | Elt.Greater -> hd :: add x tl
	  
  let rec member x s = match s with
    | [] -> false
    | hd::tl -> match Elt.compare x hd with
        | Elt.Equal   -> true     (* x belongs to s *)
        | Elt.Less    -> false    (* x is smaller than all elements of s *)
        | Elt.Greater -> member x tl
end;;


