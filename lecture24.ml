(*Final Lecture- Review (December 3, 2015)
COMP 302 gives you a different way to look at problems.

COMP 302 is introducing you to fundamental concepts found in many languages.
*Types
*Polymorphism
*Higher-order functions
*Staged programming and partial evaluation  (eg power, horribleComputation)
*State-full vs state-free computation (stateful -> references) 
*Modelling objects and closure
*Exceptions
*Continuations to defer control (exceptions with failure continuations, turn any function into tail recursive, eg append, how to use
success continuations, eg regular expression matcher and traversing tree to know where particular expression was in hw)
*Lazy programming
*Modules (devise good way of structuring, maintain usability and hiding functionality)
*)


(*HIGHER ORDER FUNCTION EXAMPLE*)
(*Write a function times whichs in an integer n and returns a function ('a -> 'a) -> 'a -> 'a*)
(*Idea: times 0   fun f x -> x
	    times 1   fun f x -> f x
	    times 2   fun f x -> f (f x)
	    ....
	    times n   fun f x -> f..(f x)
	                           ^ n	
*)

let rec times n = if n = 0 then
   fun f x -> x
else fun f x -> f(times(n-1) f x)

let rec times n = if n = 0 then
  fun f x -> x
else let r = times (n-1) in
   fun f x -> f (r f x)

  (*
Write a function monitor which takes in a function f: int -> 'a and returns
a record with 3 fields: 
numCalls      unit -> string, prints back how often execute was used
execute:      int -> 'a, keeps track that f was called and calls f on that input x
reset         unit -> unit, resets everything
  *)

let monitor f = 
let c = ref 0 in
{
	numCalls = (fun () -> ("#" string_int !c))
	execute = ((fun x -> c = !c + 1; f x)
	reset = fun() -> c = 0
}

(*
COMP 302 is learning to reason about programs.
*Induction
*Type systems provide a simple, static, lightweight tool to reason about programs approximating run-time behaviour (on midterm, will also be on final)
=> Be able to infer the most general type
*Subtyping, polymorphism
*Evaluation (Environment diagrams, operational semantics)

Typed functional programming enforces discilined programming

*)

(*PROOF EXAMPLE*)
let rec sum t = match t with
   | Empty -> 0
   | Node (x, l, r) -> x + sum l + sum r

let rec sum' t acc = match t with
   | Empty -> acc
   | Node (x, l, r) -> sum' l (x + sum'r acc)
(*Prove they are the same for practice*)

(*Infer types*)
let foo = fun f -> fun (g, x) -> g(f x)

(*    ('a -> 'c) -> ((c->d) * 'a) -> d     *)

(*
COMP 302 introduces you to fundamental principles in programming language design and how to realize these ideas in code
*How do we formally describe the grammar of a language?
*When is a variable free? When is an expression closed?
*How do we formally describe its execution? What does it mean for a language to be type safe?
*What are good langages? (really care about the semantics, how do they execute)

Why is this important?
*Gained a deeper understanding of the behaviour of a given program
*Be able to design your own little language
*Be able to implement compiler

A glimpse to the past (www.tiobe.com) -> take a look at the vast numbers of languages
*Tracks the popular programming languages

REVIEW LECTURE WITH VINCENT (TA) ON DECEMBER 16TH
FINAL: 18TH DECEMBER

PRACTICE

Functions: take a few functions from the OCaml List module (e.g. map, filter, for_all, etc.) and re-implement them yourself. Try to write an equivalent functions using continuations.

Languages: Add function expressions (fun x -> expr) and function application (e1 e2) to our small language and define the rules for free variables, substitution, type checking and evaluation.

Higher order functions and streams: implement the function zipWith: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream (take the elements of two streams, apply a function to them and put that result in a new stream)

Types and pattern matching: Here's a definition for natural numbers:

type nat = Zero | Succ of nat

Write functions like int_to_nat : int -> nat, add : nat -> nat -> nat, mult : nat -> nat -> nat, etc.

EDIT: It's going to be easier if your definition of a function is fun x:T -> e (i.e. explicitly specify the type)
*)
