(* Lecture: Continuations 
   Course : COMP 302: Programming Languages and Paradigms
   Copyright © 2015 Brigitte Pientka                                        

   Regular expression matching is a very useful technique for
   describing commonly occurring patterns.

   This program follows ideas from "Proof-directed debugging", R. Harper,
   Journal of Functional Programming 1(1), Jan 1993 

  We will give an algorithm for a simple regular expression matcher.

  Patterns describable by regular expressions:
  - Singelton : matching a specific character
  - Alternation: choice between two patterns
  - Concatenation: succession of patterns
  - Iteration: indefinite repetition of patterns


 Note: regular expressions provide no concept of 
 nesting of one pattern inside another. For this we require a richer
 formalism, namely context-free language.

 We can describe regular expressions via a BNF grammar, inductively:

  r ::= a | r1 r2 | 0 | r1 + r2 | 1 | r*

 we say a string s matches a regular expression r iff s in L(r). 

s never matches 0;
s matches 1 only if s = empty 
s matches a iff s = a
s matches r1 + r2 iff either s matches r1 or r2
s matches r1 r2 iff s = s1 s2 where s1 matches r1 and s2 matches r2.
s matches r* iff either s = empty or s = s1 s2
                 where s1 matches r and s2 matches r*

*)


type regexp = 
  Char of char | Times of regexp * regexp | One | Zero |
  Plus of regexp * regexp | Star of regexp


let rec acc r clist k = match r , clist with 
  | Char c        , []    -> false
  | Char c        , c1::s -> (c = c1) && (k s)
  | Times(r1, r2) , s     -> acc r1 s (fun s' -> acc r2 s' k)
  | One           , s     -> k s
  | Plus(r1, r2)  , s     -> acc r1 s k || acc r2 s k
  | Zero          , s     -> false
  | Star r        , s     ->  
    (k s) || acc r s (fun s' -> not(s = s') && acc (Star r) s' k)


(* ------------------------------------------------------------------------ *)
(* Auxiliary functions to turn strings into lists of characters             *)
(* ------------------------------------------------------------------------ *)
let rec tabulate f n = 
  let rec tab n acc = 
    if n = 0 then (f 0)::acc
    else tab (n-1) ((f n)::acc)
  in
  tab n []

(* string_explode : string -> char list *)
let string_explode s = 
  tabulate (fun n -> String.get s n) ((String.length s) - 1)

(* string_implode : char list -> string *)
let string_implode l = 
  List.fold_right (fun c s -> Char.escaped c ^ s) l ""

(* ------------------------------------------------------------------------ *)

(* accept : regexp * string -> bool *)
let accept r s = acc r (string_explode s) (fun l -> l = []) 


(* ------------------------------------------------------------------------ *)
(* Some examples *)
(* r0 = a + 0 *)
let r0 = Plus(Char 'a', Zero)

let ra = (Times(Char 'b', Times (Star(Char 'a'), Star(Times(Char 'a', Char 'b')))))

(* r1 = aa *)
let r1 = Times(Char 'a', Char 'a')

(* r2 = (a + b)^*   *)
let r2 = Star(Plus(Char 'a', Char 'b'))

(* r = r2 r1 r2 *)
let r3 = Times(r2, Times(r1, r2))


(* r = (a + 1)(b + ba)^* *)
let r4 = Times(Plus(Char 'a', One), Star(Plus(Char 'b',
					   Times(Char 'b',
						 Char 'a'))))
(* r = g(1+r)(e+a)y *)
let r5 = Times(Char 'g', Times(Plus(One, Char 'r'), 
				 Times(Plus(Char 'e',
					    Char 'a'),Char 'y')))

let ob = Times (Char 'o', Char 'b')
let oba = Times (Times (Char 'o', Char 'b'), Char 'a')
let oa =  Times (Char 'o', Char 'a')
let oo =  Times (Char 'o', Char 'o')

(* r = b((ob)0 + oba *)
let r5' = Times(Char 'b', Plus (Times (ob, Zero), oba))


(* r = g(1+o)^* gle *) 
let r6 = Times(Char 'g', Times(Star(Plus(One, Char 'o')), 
				 Times(Times(Char 'g', Char 'l'),Char 'e'))) 

let r6' = Times(Char 'g', Times(Star(Char 'o'), 
				 Times(Times(Char 'g', Char 'l'),Char 'e'))) 

(*
 accept r6 'google'
 accept r6 'gogle'
 accept r6 'ggle'
 accept r6 'gooooogle'
 accept r6 'gooooogle'



 accept r5 'gay'
 accept r5 'gray'
 accept r5 'grey'

*)

 (* apple, apply, ale, aply, aple, appppppple *)
let r7 = Times(Char 'a', Times(Star(Char 'p'), 
				 Times(Char 'l',
				       Plus(Times(Char 'e', Plus(One, Char 's')), Char 'y'))))






