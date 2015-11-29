(* Sacha Saint-Leger 260473392 *)

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

  let rec fold_right f l a = match l with
    | [] -> a
    | h::t -> f h (fold_right f t a)
    
  let rec map f l = match l with
    | [] -> []
    | h :: t -> (f h) :: map f t
			     
  let rec concat ll = match ll with
    | [] -> []
    | h :: t -> h @ concat t
    
  let rec filter p l = match l with
      | [] -> []
      | h :: t -> if p h then h :: filter p t else filter p t
	      
  let rec check_conditions cond l = match cond with
    | [] -> true
    | h::t -> if h l then check_conditions t l else false

  let rec return_first cond ll = match ll with
    | [] -> []
    | h :: t -> if (check_conditions cond h) then h else return_first cond t
						      
  let combinations l =
    fold_right (fun h t -> concat (map (fun h1 -> 
                                              map (fun t1 -> h1 :: t1) t) h)) l [[]];;

  let find (l: ('a input) list) (cond : ('a condition) list) =
    let perms = combinations l in
    return_first cond perms
		 
  let find_all (l: ('a input) list) (cond : ('a condition) list) = 
    let perms = combinations l in
    filter (check_conditions cond) perms;;
  
end
