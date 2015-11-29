(* Lecture: Lazy Programming
   Course : COMP 302: Programming Languages and Paradigms
   Copyright © 2015 Brigitte Pientka                                        

   Code developed as part of the lectures on foundations of programming languages

*)

type var = string
type exp = 
  | Float of float
  | Bool  of bool
  | Plus  of exp * exp 
  | Times of exp * exp 
  | Eq of exp * exp 
  | Lt of exp * exp 
  | Minus of exp * exp 
  | If_Then_Else of exp * exp * exp
  | Let of  exp * (var * exp)
  | Var of var


let e1 = Plus(Float 2.0, Times(Float 3.2, Float 4.2))
let e2 = Times(Plus (Float 2.0, Float 3.2), Float 4.2)
let e3 = Plus (Bool true, Float 2.2)
let e4 = If_Then_Else(Eq(Float 2.0, Float 2.1), 
                      Lt(Float 2.0, Float 4.2), Float 55.5)


exception Error of string 

(*  *)
let rec free_vars e = match e with 
  | Bool b -> []
  | Float n -> [] 
  | Plus(e1, e2) -> union (free_vars e1) (free_vars e2)
  | Var x -> [x] 
  | Let(e1, (x,e2)) -> union (free_vars e1) (remove x (free_vars e2))
  | _ -> raise (Error "And so it goes ... ")

(* TO DO *)
let rec subst (e',x) e = e 

let combine v1 v2 op = (match v1, v2 with 
  | Float f1, Float f2 -> Float (op f1 f2)  
  | _       , _        -> raise (Error "You are crazy!"))

let rec eval e = match e with 
  | Float n -> Float n
  | Bool b  -> Bool b
  | Plus(e1, e2) ->
      let v1 = eval e1 in 
      let v2 = eval e2 in
	combine v1 v2 (+.)
  | Times (e1, e2) ->
      let v1 = eval e1 in 
      let v2 = eval e2 in
  	combine v1 v2 ( *. )
  | If_Then_Else (e, e1, e2) -> 
      (match eval e with 
	 | Bool false -> eval e2
	 | Bool true  -> eval e1
	 | _          -> raise (Error "You are certainly crazy!"))
  | Let(e1, (x, e2)) -> 
      let v1 = eval e1 in 
	eval (subst (v1, x) e2)



  let e1 = Plus(Float 3.0, Plus (Float 2.0, Float 4.0))
  let e2 = Plus(Bool true, Plus(Float 2.0, Float 4.0))

  let e3 = If_Then_Else(Eq(Float 2.0, Float 1.0), 
			e1, 
			Lt(Float 1.0, Float 2.0))

  let e4 = Eq (If_Then_Else(Eq(Float 2.0, Float 1.0), e1, Float 55.5),
	       Float 2.2)

  
  type typ = BOOL | FLOAT

  (* infer: exp -> typ *)
  let rec infer e = match e with 
    | Float n -> FLOAT
    | Bool b  -> BOOL
    | Plus(e1, e2) | Times(e1, e2) | Minus(e1,e2) 
      -> (match infer e1, infer e2 with 
	| FLOAT, FLOAT -> FLOAT
	| _    , _     -> raise (Error "Ill-Typed!"))
    | Eq(e1, e2) -> 
	(if infer e1 = infer e2 then BOOL
	 else raise (Error "Ill-Typed!"))

    | Lt(e1, e2) ->  (match infer e1, infer e2 with 
	| FLOAT, FLOAT -> BOOL
	| _    , _     -> raise (Error "Ill-Typed!"))

    | If_Then_Else(e, e1, e2) -> (match infer e with 
        | BOOL -> 
	    let t = infer e1 in 
	    if t = infer e2 then t
	    else raise (Error "Ill-Typed!")
	| _   -> raise (Error "Ill-Typed!")
				 )



