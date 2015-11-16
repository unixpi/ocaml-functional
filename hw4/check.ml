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

  let find (l: ('a input) list) (cond : ('a condition) list) = 
    raise Error

  (* find all input combinations that satisfy a given list of conditions *)
  let find_all (l: ('a input) list) (cond : ('a condition) list) = 
    raise Error

end
