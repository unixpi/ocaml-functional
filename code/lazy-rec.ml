(* Lecture: Lazy Programming
   Course : COMP 302: Programming Languages and Paradigms
   Copyright © 2015 Brigitte Pientka                                        

  - Using higher-order functions and data-types to model lazy
   functional programming

  - Modelling infinite objects via observations.

*)

(* ---------------------------------------------------- *)
(* Suspended computation : we can suspend computation
   by wrapping it in a closure. *)
type 'a susp = Susp of (unit -> 'a)

(* force: *)
let force (Susp f) = f ()

(* ---------------------------------------------------- *)
(* Define an infinite stream via observations we can make 
   about it.

   We also call sometimes infinite objects = 
   coinductive objects.
  
   There is a finite set of experiments (observations)
   we can perform
   
    Similar to functions: we cannot directly pattern match
    on functions; but we can observe their behavior. 
    A function is a black box. We can apply it to an argument
    (experiment) and observe the result.

    This view is grounded in deep principles of type theory and 
   category theory.

*)

type 'a str = {hd: 'a  ; tl : ('a str) susp} 

let rec ones = {hd = 1 ; tl = Susp (fun () -> ones)}

(* val numsFrom : int -> int str *)
let rec numsFrom n = 
{hd = n ; 
 tl = Susp (fun () -> numsFrom (n+1))}

let nats = numsFrom 0

(* ---------------------------------------------------- *)
(* Inspect a stream up to n elements 
   val take_str : int -> 'a str -> 'a list
*)
let rec take_str n s = match n with 
  | 0 -> []
  | n -> s.hd :: take_str (n-1) (force s.tl)

(* printn: 'a str * int -> 'a list *)
let rec printn s n = if n = 0 then print_string "\n"
  else (print_string ((string_of_int s.hd) ^ " "); 
	printn (force s.tl) (n-1))

(* ------------------------------------------------------- *)
(* Recursive function returning a stream dropping the
   first n elements from stream s 

  val stream_drop : int -> 'a str -> 'a str
*)
let rec stream_drop n s = if n = 0 then  s 
  else stream_drop (n-1) (force s.tl) 

(* ---------------------------------------------------- *)
(* smap: ('a -> 'b) -> 'a str -> 'b str *)
let rec smap f s = 
{ hd = f (s.hd) ; 
  tl = Susp (fun () -> smap f (force s.tl))
}

(* ---------------------------------------------------- *)
(* Adding two streams really lazily
   val addStreams : int str -> int str -> int str
  *)
let rec addStreams s1 s2 = 
{hd = s1.hd + s2.hd ; 
 tl = Susp (fun () -> addStreams (force s1.tl) (force s2.tl)) 
}

(* ---------------------------------------------------- *)
(* val filter_str : ('a -> bool) -> 'a str -> 'a str
   val find_hd : ('a -> bool) -> 'a str -> 'a * 'a str susp 
 *)
let rec filter_str p s = 
  let h,t = find_hd p s in 
  {hd = h;
   tl = Susp (fun () -> filter_str p (force t))
  }
and find_hd p s = 
if  p (s.hd) then (s.hd, s.tl)
  else find_hd p (force s.tl)

(* Note: find_hd is NOT productive! Hence, filter is not productive.
*)

let evens = filter_str (fun x -> (x mod 2) = 0) (force (nats.tl))

let odds = filter_str (fun x -> (x mod 2) <> 0) nats 


(* ------------------------------------------------------- *)
(* Let "fibs()" represent the infinite sequence of Fibonacci numbers. Then,
   schematically, we can write:

      fibs() = 0 1 add(fibs(), tl(fibs()))

   It is almost what we can write in OCaml using observations.

Construct a stream of the fibonacci numbers -

ROUGHLY
  a      b        Fib stream
  0...   1...     0, ...
  1...   1...     0, 1 ...
  1...   2...     0, 1, 1, ...
  2...   3...     0, 1, 1, 2, ....
  3...   5...     0, 1, 1, 2, 3, ...
  5...   8...     0, 1, 1, 2, 3, 5, ...
  8...  13...     0, 1, 1, 2, 3, 5, 8 ...

*)

let rec fibs = 
{ hd = 0 ; 
  tl = Susp (fun () -> fibs') }
and fibs' = 
{hd = 1 ;
 tl = Susp (fun () -> addStreams fibs fibs')
}


(* Sequent of 1 3 9 27 .. *)
let rec fseq n k = 
  { hd = n ; 
    tl = Susp (fun () ->  fseq (n*k) k) }

(* Sequent 1, 1/2, 1/4, 1/8, 1/16 ... *)
let rec geom_series x = 
  { hd = (1.0 /. x) ;
    tl = Susp (fun () -> geom_series (x *. 2.0)) }

let rec pow n  x = if n = 0 then 1
  else x * pow (n-1) x

(* Power series 1, 2/3, 4/9, 8/27, ... = series (2^x_i / 3^x_i *)
let rec power_series x =  
{ hd = (float (pow x 2)) /. (float (pow x 3)) ; 
  tl = Susp (fun () -> power_series (x+1)) }


(*----------------------------------------------------------------------------*)
(* Sieve of Eratosthenes
 
   Idea: We begin with a stream of natural numbers starting from 2, i.e.
       2 3 4 5 6 .....
   
   - the first prime number we encounter is 2 (i.e. the head of the stream)
   - given the remaining stream 3 4 5 6 ... 
     we first remove all numbers which are dividable by 2 obtaining a stream
     3 5 .... we then call sieve again to obtain the stream of prime numbers

Example:

To find all the prime numbers less than or equal to 30 (and beyond), proceed as follows.

First generate a list of integers from 2 to 30 and beyond

 2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 ...

First number in the list is 2; this is the first prime number;
cross out every 2nd number in the list after it (by counting up in
increments of 2), i.e. all the multiples of 2:  

   3  5  7 9 11 13 15 17 19 21 23 25 27 29  ...

   Next prime number after 2 is 3; 
   now cross out every number which can be divided by 3, i.e. all the
   multiples of 3: 

   5 7 11 13 17 19 23 25 29  ...

   Next prime number is 5; cross out  all the multiples of 5:

   7 11 13 17 19 23 29  ...

   Next prime number is 7;  the next step would be to cross out all
   multiples of y; but there are none. In fact, the numbers left not
   crossed out in the list at this point are all the prime numbers
   below 30; 

   7 11 13 17 19 23 29  ...
*)

let no_divider m n = not (n mod m = 0)
(* val sieve : int str -> int str *)
let rec sieve s = 
{ hd = s.hd ; 
  tl = Susp (fun () -> sieve (filter_str (no_divider s.hd) (force s.tl))) 
}

let nats2 = stream_drop 2 nats 

let primes = sieve nats2

