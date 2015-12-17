(* -----------------------------------------------------------------*)
(* Exceptions 

Author: Brigitte Pientka
Course: COMP 302, McGill University, Montreal, Canada
Copyright © 2012 Brigitte Pientka

OCaml as any other flavor of the ML-language family is a safe language. 
This is ensured by static type checking and by
dynamic checks that rule out violations that cannot be detected
 statically. 

Examples: division by zero, arithmetic overflow

Static violations are signalled by type checking error.
Dynamic violations are signalled by raising an exception.

For example: 

3 / 0 will type-check but cannot be evaluated and will incur a
 runtime fault that is signalled by raising the exception Division_by_zero

An exception is a form of answer to the query: what is the value of
 this expression?

OCaml will report this error as follows:

# 3 / 0;;
Exception: Division_by_zero.
# 

Another source of dynamic run-time error is due to non-exhaustive
 matches. For example:

let head (x::t) = x ;;
Characters 9-19:
  let head (x::t) = x ;;
           ^^^^^^^^^^
Warning P: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val head : 'a list -> 'a = <fun>
# head [];;
Exception: Match_failure ("", 650, -489).
#  

A related situation arises when you pattern match against a specific
pattern in a let or case construct.

# let test l = let h:: _ = l in h;;
Characters 17-22:
  let test l = let h:: _ = l in h;;
                   ^^^^^
Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
[]
val test : 'a list -> 'a = <fun>
# test [];;
Exception: Match_failure ("//toplevel//", 286, -7548).

So far we have considered examples of pre-defined exceptions.Since
built-in exceptions have a built-in meaning, we can in fact introduce
new excpetions to signal program specific errors.

*)

(* -----------------------------------------------------------------*)
(* We have briefly seen the use of user-defined exceptions already...
   Here is the example we have seen earlier *)

exception Domain

(*
   Check the invariant for the externally
   visible function only, and not during recursion.
*)

let fact n =
  let rec f n = 
    if n = 0 then 1
    else n * f (n-1)  
  in
    if n < 0 then raise Domain
    else f(n)

let runFact n = 
  try
    let r = fact n in 
      print_string ("Factorial of " ^  string_of_int n ^ " is " ^ string_of_int r ^ "\n")
  with Domain -> print_string "Error: Invariant violated -- trying to call factorial on inputs < 0 \n"

(*
# runFact 3;;
Factorial of 3 is 6
- : unit = ()

# runFact (-3);;
Error: Invariant violated -- trying to call factorial on inputs < 0 
- : unit = ()
# 
*)
(* -----------------------------------------------------------------*)
(* Define an program-specific exception with a name *)

exception NotFound

(* Using exceptions to signal error *)

type key = int

type 'a btree = 
  | Empty 
  | Node of 'a btree * (key * 'a) * 'a btree


(* Previous solution to checking whether there is an element with key
   k in the tree provided based on option type; 

   findOpt : 'a btree -> int -> 'a opt *)
let rec  findOpt t k = match t with 
  | Empty -> None
  | Node(l, (k',d), r) -> 
      if k = k' then Some d
      else 
	(if k < k' then findOpt l k else findOpt r k)

(* New implementation using exception *)

(* Find (T,k) = d if (k,d) is in the tree T
   raises exception notFound otherwise *)
let rec find t k = match t with 
  | Empty -> raise NotFound
  | Node (l, (k',d), r) ->
    if k = k' then d
    else 
      (if k < k' then find l k else find r k)

(* The use of exceptions to signal error conditions sugests raising
 exceptions is fatal: execution of the program terminates with the
 raised exception  -- but signalling an error is only one use of
 exceptions! 

 We may want to propagate a value together with the exception!

*)


(* Find t k n = d 
   searches the tree T up to depth n. If we did no find an entry with key k
   we raise the exception Error with error msg "No entry found in tree"; 
   if we exceeded our bound n, we raise the exception Error with the
   error msg "Bound exceeded" .


*)

exception Error of string 

let rec findEx t k n = 
  if n <= 0 then raise (Error "Bound exceeded")
  else 
    match t with 
      | Empty -> raise (Error ("No entry with key " ^ 
			  string_of_int k ^ 
			  " found in tree"))
      | Node (l, (k',d), r) -> 
	  if k = k' then d
	  else 
	    (if k < k' then findEx l k (n-1) else findEx r k (n-1))


(* Alternatively, we could also simply define a new kind of an 
   exception called BoundExceeded to distinguish this case from
   the case where we did not find an entry *)

exception BoundExceeded
let rec findEx' t k n = 
  if n <= 0 then raise BoundExceeded
  else 
    match t with 
      | Empty -> raise NotFound
      | Node (l, (k',d), r) -> 
	  if k = k' then d
	  else 
	    (if k < k' then findEx' l k (n-1) else findEx' r k (n-1))

(* -----------------------------------------------------------------*)

(* Some top level functions which illustrate how to
 handle exceptions; we contrast it to how to handle
 optional value *)

let findOpt_top t k = 
  match findOpt t k with
  | None   -> print_string ("Found no element with key " ^ string_of_int k ^ " in tree \n")
  | Some d -> print_string ("Found element " ^ string_of_int d ^ " in tree \n")

(* -----------------------------------------------------------------*)
(* HANDLING OF EXCEPTIONS 
 
   try exp1 with exc_pat -> exp 2

   installs an exception handler.

   This allows for non-local transfer of control. We may catch a
   raised exception and continue evaluating along some other path.

   OCaml attempts to evaluate the expression exp1. If this yields a value, then this
   value is returned. If evaluation of part of expression exp1 raises an exception exc
   then evalution of exp1 will stop possibly midway and exc is matched against exc_pat
   to determine how to proceed.  

   if there is no handler or no successful match, the exception
   will re-raised and propagated to the next higher level.

   If it is never caught at any higher level, the computation will
   abort with an uncaugt exception exc

*)


let find_top t k = 
  try
    let d = find t k in  
      (* the exception raised by find(T,k) will propel you 
	 to the next higher level! -- nothing below this line
	 in particular the statement print "Found element ..."
	 will not be executed *)
      print_string ("Found element " ^ string_of_int d ^ " in tree \n")
  with NotFound ->  print_string ("Found no element with key " ^ string_of_int k ^ " in tree \n")


(* we can pattern match on exceptions *)
let find_toplevel t k = 
 try
   findEx t k 4
 with 
   | Error msg ->  print_string msg

(* we can also dispatch on the different exceptions *)
let find_top' t k = 
 try
   findEx' t k 4
 with 
   | BoundExceeded ->  print_string ("Bound exceeded  \n")
   | NotFound ->  print_string ("Found no element with key " ^ string_of_int k ^ " in tree \n")
     
(* -----------------------------------------------------------------*)
(* Exceptions are propogated to the next level, if they are not caught! *)
let find_uncaught t k = find t k

let find_catch t k = 
 try 
   let d = find_uncaught t k in 
   print_string ("Found element " ^ string_of_int d ^ " in tree \n")
 with NotFound -> print_string ("Found no element with key " ^ string_of_int k ^ " in tree \n")

(* The primary benefit of exceptions:
 1 The force you to consider the exceptional case.
 2 They allow you to segregate the special case 
   from the normal case in the code.
   (often avoids clutter)
*)
(* -----------------------------------------------------------------*)
(* USING EXCEPTIONS FOR BACKTRACKING - Diverting the control flow                    *)
(* Use of exceptions to initiate backtracking in a tree
   which is not necessarily a binary search tree *)
let rec find_gen  t k = match t with
  | Empty -> raise NotFound
  | Node (l, (k',d), r) -> 
      if k = k' then d 
      else 
	try find_gen l k with NotFound -> find_gen r k
(* -----------------------------------------------------------------*)
(*
Consider what happens when we evaluate and understand how
exception handlers are installed.

l = Node ( Node (Empty, (3, "3"), Empty)  , (7, "7"), Node (Empty, (44, "44"), Empty)), 
r = Node ( Node (Empty, (55, "55"), Empty)  , (8, "8"), Empty
t = Node(
      l      
      (1, "1"), 
      r), 

find_gen t 55
==> try find_gen l 55 
    with NotFound -> find_gen r 55
==> try 
       (try find_gen (Node (Empty, (3, "3"), Empty) )
        with NotFound -> find_gen (Node (Empty, (44, "44"), Empty) ))
    with NotFound -> find_gen r 55

==> try 
       (try 
          (try find_gen  Empty with NotFound -> Empty)
        with NotFound -> find_gen (Node (Empty, (44, "44"), Empty) ))
    with NotFound -> find_gen r 55

==> try  find_gen (Node (Empty, (44, "44"), Empty) )
      with NotFound -> find_gen r 55

...
  

*)


(* -----------------------------------------------------------------*)
(* Making change with given coins
   [5;2] means we have an infinite number of 5 and 2 coins.

# change [50;25;10;5;2;1] 43;;
- : int list = [25; 10; 5; 2; 1]
# change [50;25;10;5;2;1] 13;;
- : int list = [10; 2; 1]
# change [5;2;1] 13;;
- : int list = [5; 5; 2; 1]
# 

The idea is to proceed greedily, but if we get stuck,
we undo the most recent greedy decision and proceed again from there.

*)
(* First some toString conversion function  as a helper function 
   this function is used for printing later on
*)
let listToString l = match l with 
  | [] -> ""
  | l -> 
    let rec toString l = match l with 
      | [h]  -> string_of_int h
      | h::t -> string_of_int h ^ ", " ^ toString t
    in
      toString l


(* Define Exception *)
exception Change

(* change: : int list -> int -> int list *)
let rec change coins amt = 
  if amt = 0 then [] 
  else 
    begin match coins with 
      | [] -> raise Change
      | coin::cs ->  
	  if coin > amt then
	    change cs amt
	  else 
	    try 
	      coin :: change coins (amt - coin)
	    with Change -> change cs amt
    end

let change_top coins amt =
  try 
    let c = change coins amt in
      print_string ("Return the following change: " ^ listToString c ^ "\n")
  with Change -> print_string ("Sorry, I cannot give change\n")

(* Here is the behavior of change_top : 

# change_top [2;5] 3;;
Sorry, I cannot give change
- : unit = ()
# change_top [2;5] 8;;
Return the following change: 2, 2, 2, 2
- : unit = ()
# change_top [25;10;5;2] 43;;
Return the following change: 25, 10, 2, 2, 2, 2
- : unit = ()
# change_top [25;10;5;2] 44;;
Return the following change: 25, 10, 5, 2, 2
- : unit = ()
# change_top [25;10;2] 44;;
Return the following change: 10, 10, 10, 10, 2, 2
- : unit = ()
# change_top [25;10;2] 43;;
Return the following change: 25, 10, 2, 2, 2, 2
- : unit = ()
# change_top [25;10;2] 23;;
Sorry, I cannot give change
- : unit = ()
# 
*)
