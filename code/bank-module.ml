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
(* The power of abstraction and information hiding *)

module type CURRENCY = 
sig 
  type t 
  val unit : t 
  val plus : t -> t -> t 
  val prod : float -> t -> t 
  val toString : t -> string
end;;

module Float = 
struct 
  type t = float 
  let unit = 1.0 
  let plus = (+.) 
  let prod = ( *. ) 
  let toString  x = string_of_float x
end;;

(* Abstraction may also be used to produce two isomorphic but
   incompatible views of a same structure. For instance, all
   currencies are represented by floats; however, all currencies are
   certainly not equivalent and should not be mixed. Currencies are
   isomorphic but disjoint structures, with respective incompatible
   units Euro and Dollar. This is modeled in OCaml by a signature
   constraint / module type. *)  

(* Remark that multiplication became an external operation on floats
   in the signature CURRENCY. Constraining the signature of Float to
   be CURRENCY returns another, incompatible view of Float. Moreover,
   repeating this operation returns two isomorphic structures but with
   incompatible types t. *) 

module Euro = (Float : CURRENCY);; 
module USD = (Float : CURRENCY);;
 	
let euro x = Euro.prod x Euro.unit;; 
let usd x = Euro.prod x Euro.unit;; 

(* In Float the type t is concrete, so it can be used for
   "float". Conversely, it is abstract in modules Euro and
   Dollar. Thus, Euro.t and Dollar.t are incompatible.  
 
# let euro x = Euro.prod x Euro.unit;; 
val euro : float -> Euro.t = <fun>
# let x = Euro.plus (euro 10.0) (euro 20.0);;
val x : Euro.t = <abstr>
# Euro.toString x;;
- : string = "30."
# Euro.toString (Euro.plus (euro 10.0) (USD.unit));;
Characters 37-50:
  Euro.toString (Euro.plus (euro 10.0) (USD.unit));;
                                       ^^^^^^^^^^^^^
Error: This expression has type USD.t
       but an expression was expected of type Euro.t
# 

NOTE: We cannot mix units! This is a good thing!

*)

(* Let us now define a bank with clients generically - 
   it can be re-used in Europe, Canada, US, ...
   the concrete currency is opaque.
*)
module type CLIENT = (* client's view *)
sig 
  type account_number       (* account *)
  type currency 
  val deposit : account -> currency -> currency 
  val retrieve : account -> currency -> currency 
  val print_balance : account -> string
end;; 

module type BANK = (* banker's view *) 
sig 
  include CLIENT   (* BANK inherits all the values declared in the
		      signature CLIENT ; 
		      if we subsequently declare names which already
		      are declared in CLIENT we are overshadowing the
		      previous names.
		   *)
  val create : unit -> account_number

end;;

(* Functor *)
module Old_Bank (M : CURRENCY) : (BANK with type currency = M.t) = 
struct 
  type currency = M.t 
  type account_number = { mutable balance : currency } 
  (* Note: we could have said type account_number = currency ref ;
     References in OCaml are in fact records with one
     mutable field "content". 
     However, it is nice to be able to refer to the reference by 
     name.
  *)
  let zero = M.prod 0.0 M.unit 
  and neg = M.prod (-1.0) 
  
  let create() = { balance = zero } 

  let deposit c x = 
    if x > zero then 
      c.balance <- M.plus c.balance x; 
      c.balance 
  
  let retrieve c x = 
    if c.balance > x then 
      deposit c (neg x) 
    else 
      c.balance 

  let print_balance c = 
    M.toString (c.balance)

end;; 


(* Illustrating inheritance and code reuse *)
module Post = Old_Bank (Euro);; 

module Post_Client : (CLIENT with type currency = Post.currency 
 			     and type account_number = Post.account_number) = 
  Post;;

(*
 This model is fragile because all information lies in the account itself. For
 instance, if the client loses his account, he loses his money as well, since
 the bank does not keep any record. Moreover, security relies on type
 abstraction to be unbreakable… 

However, the example already illustrates some interesting benefits of
modularity: the clients and the banker have different views of the bank
account. As a result an account can be created by the bank and used for
deposit by both the bank and the client, but the client cannot create new
accounts.  
*)

(*     	 	

# let my_account = Post.create ();;
val my_account : Post.t = <abstr>
# Post.deposit my_account (euro 100.0);;
- : Post.currency = <abstr>
# Post.print_balance my_account;;
- : string = "100."
# Post_Client.deposit my_account (euro 100.0);;
- : Post_Client.currency = <abstr>
# Post.print_balance my_account;;
- : string = "200."
# Post_Client.print_balance my_account;;
- : string = "200."
# 

 Moreover, several accounts can be created in different currencies, with no possibility to mix one with another, such mistakes being detected by typechecking.
*)
  	 	
module Citybank = Old_Bank (USD);; 
let my_dollar_account = Citybank.create();;

(*
# Citybank.deposit my_account;;
Characters 17-27:
  Citybank.deposit my_account;;
                   ^^^^^^^^^^
Error: This expression has type Post.t = Old_Bank(Euro).t
       but an expression was expected of type Citybank.t = Old_Bank(USD).t

# Citybank.deposit my_dollar_account (euro 100.0);;
Characters 35-47:
  Citybank.deposit my_dollar_account (euro 100.0);;
                                     ^^^^^^^^^^^^
Error: This expression has type Euro.t but an expression was expected of type
         Citybank.currency = USD.t
# 
*)
(* Furthermore, the implementation of the bank can be changed while preserving its interface. We use this capability to build, a more robust —yet more realistic— implementation of the bank where the account book is maintained in the bank database while the client is only given an account number. *)
    	 	
module Bank (M : CURRENCY) : (BANK with type currency = M.t) =  
struct 
  let zero = M.prod 0.0 M.unit  
  and neg = M.prod (-1.0) 
  
  type account_number = int  
  type currency = M.t 
  type bank_account = { number : int; mutable balance : currency } 

  (* bank database *)  
  let all_accounts = Hashtbl.create 100
  and last = ref 0  

  let account n = Hashtbl.find all_accounts n  

  let create() =  
    let n = incr last; !last  
    in  
    Hashtbl.add all_accounts n {number = n; balance = zero};  
    n  

  let deposit n x =  
    let c = account n in 
    if x > zero then 
      c.balance <- M.plus c.balance x; 
      c.balance 

  let retrieve n x = 
    let c = account n in 
    if c.balance > x then (c.balance <- M.plus c.balance x; x) else zero 

  let print_balance n = 
    let c = account n in 
    M.toString (c.balance)

end;; 

(* Using functor application we can create several banks. As a result of
   generativity of function application, they will have independent and private
   databases, as desired.

  Each defining occurrence of Bank (Euro) with a unique internal stamp and
  generates a new distinct (copy of the) module.
   
 *)
    	 	
module Central_Bank = Bank (Euro);; 
module Banque_de_France = Bank (Euro);;

(* Furthermore, since the two modules Old_bank and Bank have the same interface, one can be used instead of the other, so as to created banks running on different models.
*)
 	 	
module Old_post = Old_Bank(Euro) 
module New_post = Bank(Euro) 
module Citybank = Bank(USD);;

(* All banks have the same interface, however they were built. In fact, it happens to be the case that the user cannot even observe the difference between either implementation; however, this would not be true in general. Indeed, such a property can not be enforced by the typechecker. 


NOTE: OCaml does not cleanly manage scope of module names. Re-defining module Post = Bank (Euro) has some unexpected behavior...


*)

