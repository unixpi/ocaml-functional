(* -------------------------------------------------------------------------- *)
(* Author: Brigitte Pientka                                                   *)
(* COMP 302 Programming Languages - FALL 2015                                 *)
(* Copyright © 2015 Brigitte Pientka                                         *)
(* -------------------------------------------------------------------------- *)  
(*
  STUDENT NAME(S): Sacha Saint-Leger
  STUDENT ID(S)  :260473392


  Fill out the template below.

*)

module type DICT =
sig
  module Elem : ORDERED 
  type dict 

  exception Error of string

  val create : unit -> dict
  val add    : dict -> Elem.t list -> dict
  val find   : dict -> Elem.t list -> bool
  val iter   : dict -> (unit -> unit) -> (Elem.t -> unit) ->  unit
  val number_of_elem: dict -> int
  val max_path: dict -> int
end;;

module DictTrie(K : ORDERED) : (DICT with type Elem.t = K.t) = 
struct 
  module Elem = K 

  exception Error  of string 

  type trie = 
    | End
    | Node of  Elem.t * dict

  (* dictionaries are ordered *)
  and dict = trie list

  let create () = []

  (* ------------------------------------------------------------------------ *)
  (* Adding a list of elements in a trie ; duplicates are not allowed.        *)
  (* add: dict -> Elem.t list -> dict                                         *)
  (* Invariant: dictionary preserves the order                                *)
  let rec list_to_trie list = match list with
    | [] -> End
    | h::t -> Node(h, [list_to_trie t])

  let hd list = match list with h::t -> h;;

  let tl list = match list with h::t -> t;;
  
  let rec add (t : dict) (l : Elem.t list)  =
    
  let rec add (t : dict) (l : char list) =
      match (hd t) (hd l) with
      | Node(elem,children) , elemi -> if elemi = elem then
                                         add children (tl l)
                                       else
                                         if elemi > elem then
                                           add (tl t) l
                                         else 
                                           (list_to_trie l) :: t;;

  (* ------------------------------------------------------------------------ *)
  (* find : Elem.t list -> dict -> bool *)
  let rec find dict elem_list = raise (Error "Not Implemented Yet")


  (* ------------------------------------------------------------------------ *)
  let rec iter dict g f = raise (Error "Not Implemented Yet")

   let number_of_elem d = raise (Error "Not Implemented Yet")

   let max_elem d = raise (Error "Not Implemented Yet")

end



module IdNumbers =  DictTrie(IntLt)
module Phrases = DictTrie(StringLt)
module Names = DictTrie(CharLt)



