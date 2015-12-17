(* Course : COMP 302: Programming Languages and Paradigms
   Copyright © 2015 Brigitte Pientka                     

   Processing finite objects lazily is also useful;
   it corresponds to demand driving compution.
*)
(* ---------------------------------------------------- *)
(* Suspended computation : we can suspend computation
   by wrapping it in a closure. *)
type 'a susp = Susp of (unit -> 'a)

(* delay: *)
let delay f = Susp(f)

(* force: *)
let force (Susp f) = f ()

(* ---------------------------------------------------- *)
(* We define next a lazy list; this list is possibly
   finite; this is accomplished by a mutual recursive
   datatype.

   'a lazy_list defines a lazy list; we can observe the 
   head and its tail. For the tail we have two options:
   we have reached the end of the list indicated by the 
   constructor Empty or we have not reached the end 
   indicated by the constructor NonEmpty and we expose
   another lazy list of which we can observe the head and the tail.  

*)
type 'a lazy_list = {hd: 'a  ; tl : ('a fin_list) susp} 
and 'a fin_list = Empty | NonEmpty of 'a lazy_list

(* We can now rewrite our previous functions on infinite 
   streams for lazy-lists

*)

(* 
   val take : int -> 'a lazy_list -> 'a list 
   val take' : int -> 'a fin_list -> 'a list 
*)
let rec take n s = match n with 
  | 0 -> []
  | n -> s.hd :: take' (n-1) (force s.tl)

and take' n s = match s with 
  | Empty -> [] 
  | NonEmpty s -> take n s

(* val map : ('a -> 'b) -> 'a lazy_list -> 'b lazy_list 
   val map' : ('a -> 'b) -> 'a fin_list -> 'b fin_list 
*)
let rec map f s = 
{ hd = f s.hd ; 
  tl = Susp (fun () -> map' f (force s.tl))
}

and map' f xs = match xs with 
  | Empty -> Empty
  | NonEmpty xs -> NonEmpty (map f xs)

(*
 val append' : 'a fin_list -> 'a fin_list -> 'a fin_list 
 val append : 'a lazy_list -> 'a fin_list -> 'a lazy_list 
*)
let rec append' s1 s2 = match s1 with 
  | Empty -> s2 
  | NonEmpty xs -> NonEmpty (append xs s2)
and append xs s2 = 
  {hd = xs.hd ; 
   tl = Susp (fun () -> append' (force xs.tl) s2)}

(* create a finite lazy list *)
(*
val natsFrom : int -> int lazy_list =
val natsFrom' : int -> int fin_list =
*)
let rec natsFrom n = 
  { hd = n ; 
    tl = Susp (fun () -> natsFrom' (n-1)) } 

and natsFrom' n = if n < 0 then Empty
  else NonEmpty (natsFrom n)

(* Alternative ...*)
(* val lazy_nats_from : int -> int fin_list *)
let rec lazy_nats_from n = match n with 
  | 0 -> Empty
  | n -> NonEmpty {hd = n ; tl = Susp (fun () -> lazy_nats_from (n-1)) }

