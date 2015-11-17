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


  (* Check whether all conditions in cond are satisfied *)
  let hd l = match l with
    | h::t -> h

  let rec check_all_conditions cond l = match cond with
    | [] -> true
    | h::t -> if h l then check_all_conditions t l else false

  let rec check_all_combinations cond a l  = match l with
    | [] -> []
    | h::t -> if (check_all_conditions cond [a;h]) then [a;h]
	      else check_all_combinations cond a t		
      
  let find (l: ('a input) list) (cond : ('a condition) list) = 
    let rec findb l cond acc = match l with
      | [[]; l2] -> []
      | [h1::t1; l2]  -> let return_val = (check_all_combinations cond h1 l2) in
			 if (return_val = []) then findb [t1;l2] cond acc
			 else return_val
    in findb l cond (fun () -> [])		  

  (* find all input combinations that satisfy a given list of conditions *)
  let find_all (l: ('a input) list) (cond : ('a condition) list) = 
    raise Error

end
