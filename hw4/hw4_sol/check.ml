module type CHECK = 
sig

  exception Error

  type 'a input = 'a list
  type 'a args = 'a list
  type 'a condition = 'a args -> bool

  val find     : 'a input list -> 'a condition list -> 'a args  
  val find_all : 'a input list -> 'a condition list -> 'a args list


end 



module Check : CHECK = 
struct

  exception Error 
  type 'a input = 'a list
  type 'a args = 'a list
  type 'a condition = 'a args -> bool

  (* Q1.1 *)				   

  (* Helper function with continuations *)
  let rec hfind (l: ('a input) list) (cond : ('a condition) list) (args : 'a args) (fail : unit -> 'a args) = match l with
    | [] -> if (List.fold_left (fun x -> fun y -> y args && x) true cond) then args else fail ()
    | [] :: b -> fail ()
    | (h :: t) :: b -> hfind b cond (args@[h]) (fun () -> hfind (t::b) cond args fail)		
				    
  let find (l: ('a input) list) (cond : ('a condition) list) = hfind l cond [] (fun () -> raise Error)

  (* Q1.2 *)

  (* Helper function with continuations *)
  let rec hfind_all (l: ('a input) list) (cond : ('a condition) list) (args : 'a args) (succ : ('a args list -> 'a args list)) (fail : unit -> 'a args list) : 'a args list = match l with
    | [] -> if (List.fold_left (fun x -> fun y -> y args && x) true cond) then (succ [args]) else fail ()
    | [] :: b -> fail ()
    | (h :: t) :: b -> hfind_all b cond (args@[h]) 
              		(fun a -> hfind_all (t::b) cond args (fun b -> succ (a@b)) (fun () -> succ a))
			 (fun () -> hfind_all (t::b) cond args succ fail)
				    
  let find_all (l: ('a input) list) (cond : ('a condition) list) = hfind_all l cond [] (fun a -> a) (fun () -> [])

end
