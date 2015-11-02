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

  let rec remove_last2 list = match list with
      | [] -> []
      |  h :: t -> if  t = [] then [] else (h :: (remove_last2 t));; 


  (* use the line below for testing with char lists *)

  let rec add (t : dict) (l : Elem.t list)  =
    (*  let rec add (t : dict) (l : char list) *)
    if t = [] then [list_to_trie l]
    else if l = [] then End :: t 		     
    else
      match (hd t) , (hd l) with
      | End, _ -> (hd t) :: (list_to_trie l) :: (tl t)
        | Node(elem,children) , elemi -> if elemi = elem then
                                           Node(elem, add children (tl l)) :: (tl t)
                                         else
                                           if elemi > elem then
                                             (hd t) :: add (tl t) l
                                           else 
                                             (list_to_trie l) :: t;;




(* testing 
 let dict = add [] ['a';'c';'e'];;
 let dict = add dict ['a';'c';'e';'a'];;
 let dict = add dict ['a';'l';'l';'e';'y'];;
 let dict = add dict ['a';'l';'e'];;
 let dict = add dict ['a';'l';'e';'x'];;

 *)
  (* ------------------------------------------------------------------------ *)
  (* find : Elem.t list -> dict -> bool *)
 

 let rec find dict elem_list  = 
   (* let rec find (dict : dict) (elem_list : char list) = *)
    if ((dict = []) && (elem_list != [])) then false else
      if (((hd dict) = End) && (elem_list = [])) then true
      else
        if (elem_list = []) then false
        else
	  match (hd dict) , (hd elem_list) with
          | End, elemf -> find (tl dict) elem_list
          | Node(elem,children), elemf -> if (elemf = elem) then
	              			    find children (tl elem_list)
				          else
				            if (elemf > elem) then
					      find (tl dict) elem_list
				            else (* elemf < elem *)
					      false;;
					


  (* ------------------------------------------------------------------------ *)
  let rec iter dict g f = raise (Error "Not Implemented Yet")

   let number_of_elem d = raise (Error "Not Implemented Yet")

   let max_elem d = raise (Error "Not Implemented Yet")

end



module IdNumbers =  DictTrie(IntLt)
module Phrases = DictTrie(StringLt)
module Names = DictTrie(CharLt)



