 module type EXP =
 sig

  type var
  type exp =
    | Plus    of exp * exp  (* Plus *)
    | Minus   of exp * exp  (* Minus *)
    | Times   of exp * exp  (* Times *)
    | Div     of exp * exp  (* Div *)
    | Eq      of exp * exp
    | Lt      of exp * exp
    | And     of exp * exp  (* log. and *)
    | Or      of exp * exp  (* log. or *)
    | Not     of exp
    | Trunc   of exp
    | ToFloat of exp
    | Int     of int
    | Float   of float
    | Bool    of bool
    | If_Then_Else of exp * exp * exp
    | Sum     of exp * exp * (var * exp)
    | Var     of var

  type tp = INT | BOOL | FLOAT

  exception Error of string

  val eval : exp -> exp
  val infer : exp -> tp

end


 module ArithExp : EXP with type var = string = 
 struct
  type var = string

  type exp =
    | Plus    of exp * exp  (* Plus *)
    | Minus   of exp * exp  (* Minus *)
    | Times   of exp * exp  (* Times *)
    | Div     of exp * exp  (* Div *)
    | Eq      of exp * exp
    | Lt      of exp * exp
    | And     of exp * exp  (* log. and *)
    | Or      of exp * exp  (* log. or *)
    | Not     of exp
    | Trunc   of exp
    | ToFloat of exp
    | Int     of int
    | Float   of float
    | Bool    of bool
    | If_Then_Else of exp * exp * exp
    | Sum     of exp * exp * (var * exp)  (* sum(lower, upper, ("x", e)) *)
    | Var     of var

  type tp = INT | BOOL | FLOAT


  exception Error of string

  let newVar =
    let counter = ref 0 in
      (fun x ->
	 let _ = counter := !counter+1 in
	 string_of_int (!counter) ^ x)

  let rec remove a l = match l with
    | [] -> []
    | h :: t -> if (h = a) then remove a t else h :: remove a t

  
  let rec remove_duplicates list = match list with
    | [] -> []
    | h :: t -> (h :: remove_duplicates (remove h t))

  let rec contains e l = match l with
    | [] -> false
    | h :: t -> if e=h then true else contains e t
 
  let rec fv (e : exp) : var list = match e with
    | Plus(e1, e2) -> remove_duplicates((fv e1) @ (fv e2))
    | Minus(e1, e2) -> remove_duplicates((fv e1) @ (fv e2))
    | Times(e1, e2) -> remove_duplicates((fv e1) @ (fv e2))
    | Div(e1, e2) -> remove_duplicates((fv e1) @ (fv e2))
    | Eq(e1, e2) -> remove_duplicates((fv e1) @ (fv e2))
    | Lt(e1,e2) -> remove_duplicates((fv e1) @ (fv e2))
    | And(e1,e2) -> remove_duplicates((fv e1) @ (fv e2))
    | Or(e1,e2) -> remove_duplicates((fv e1) @ (fv e2))
    | Not(e1) -> remove_duplicates(fv e1)
    | Trunc(e1) -> remove_duplicates(fv e1)
    | ToFloat(e1) -> remove_duplicates(fv e1)
    | Int n -> []
    | Float n -> []
    | Bool p -> []
    | If_Then_Else(e1,e2,e3) -> remove_duplicates((fv e1) @ (fv e2) @ (fv e3))
    | Sum(e1,e2,(x,e3)) -> remove_duplicates((fv e1) @ (fv e2) @ (remove x (fv e3)))
    | Var x -> [x]


  let rec subst (e,x : exp * var) (e' : exp) : exp = match e' with
    | Plus(e1, e2) -> Plus(subst (e,x) e1, subst (e,x) e2)
    | Minus(e1, e2) -> Minus(subst (e,x) e1, subst (e,x) e2)
    | Times(e1, e2) -> Times(subst (e,x) e1, subst (e,x) e2)
    | Div(e1, e2) -> Div(subst (e,x) e1, subst (e,x) e2)
    | Eq(e1, e2) -> Eq(subst (e,x) e1, subst (e,x) e2)
    | Lt(e1,e2) -> Lt(subst (e,x) e1, subst (e,x) e2)
    | And(e1,e2) -> And(subst (e,x) e1, subst (e,x) e2)
    | Or(e1,e2) -> Or(subst (e,x) e1, subst (e,x) e2)
    | Not(e1) -> Not(subst (e,x) e1)
    | Trunc(e1) -> Trunc(subst (e,x) e1)
    | ToFloat(e1) -> ToFloat(subst (e,x) e1)
    | Int n -> Int n
    | Float n -> Float n
    | Bool p -> Bool p
    | If_Then_Else(e1,e2,e3) -> If_Then_Else(subst (e,x) e1, subst (e,x) e2, subst (e,x) e3) 
    | Sum(e1,e2,(y,e3)) -> if ( contains x (fv (Sum(e1,e2,(y,e3))))) then 
                             (Sum(subst (e,x) e1, subst (e,x) e2, (y, subst (e,x) e3)))
			   else
			     (Sum(subst (e,x) e1, subst (e,x) e2, (y, e3)))
			     
    | Var y -> if y = x then e else Var y

  let combineFloats v1 v2 op = match v1, v2 with
    | Float f1, Float f2 -> Float (op f1 f2)
    | _       , _        -> raise (Error "You are crazy!")

  let combineInts v1 v2 op = match v1, v2 with
    | Int i1, Int i2 -> Int (op i1 i2)
    | _, _ -> raise (Error "you are fucking nuts sir!")

  let rec eval (e : exp) : exp = match e with
    | Int n -> Int n
    | Float n -> Float n
    | Bool p -> Bool p
    | Plus(e1, e2) -> (let v1 = eval e1 in
		       let v2 = eval e2 in
		       try
			 combineFloats v1 v2 (+.)
		       with
			 _ -> combineInts v1 v2 (+))
    | Minus(e1, e2) -> (let v1 = eval e1 in
		       let v2 = eval e2 in
		       try
			 combineFloats v1 v2 (+.)
		       with
			 _ -> combineInts v1 v2 (+))
    | Times(e1, e2) -> (let v1 = eval e1 in
		       let v2 = eval e2 in
		       try
			 combineFloats v1 v2 (+.)
		       with
			 _ -> combineInts v1 v2 (+))
    | Div(e1, e2) ->  (let v1 = eval e1 in
		       let v2 = eval e2 in
		       try
			 combineFloats v1 v2 (+.)
		       with
			 _ -> combineInts v1 v2 (+))
    | If_Then_Else(e1,e2,e3) -> (match eval e1 with 
	 | Bool false -> eval e3
	 | Bool true  -> eval e2
	 | _          -> raise (Error "You are certainly crazy!"))

    | Eq(e1, e2) -> 
    | Lt(e1,e2) -> 
    | And(e1,e2) ->
    | Or(e1,e2) -> 
    | Not(e1) -> (match eval e1 with
		  | Bool false -> Bool true
		  | Bool true -> Bool false
		  | _ -> raise (Error "You are certainly crazy!"))
    | Trunc(e1) -> 
    | ToFloat(e1) -> 

    | Sum(e1,e2,(x,e3)) -> 
    | Var x ->

  let rec contains1 tuplelist = match tuplelist with
    | [] -> raise (Error "Variable Unbound")
    | (v1,tp) :: tl -> if (v = v1) then tp else contains1 v tl 
				     
  let rec infer (ctx : (var * tp) list) (e : exp) : tp = match e with
    | Plus(e1, e2) -> (match infer ctx e1, infer ctx e2 with
		       | FLOAT, FLOAT -> FLOAT
		       | INT, INT -> INT
		       | _ , _ -> raise (Error "Ill-Typed!"))
    | Minus(e1, e2) ->  (match infer ctx e1, infer ctx e2 with
		       | FLOAT, FLOAT -> FLOAT
		       | INT, INT -> INT
		       | _ , _ -> raise (Error "Ill-Typed!"))
    | Times(e1, e2) -> (match infer ctx e1, infer ctx e2 with
		       | FLOAT, FLOAT -> FLOAT
		       | INT, INT -> INT
		       | _ , _ -> raise (Error "Ill-Typed!"))
    | Div(e1, e2) -> (match infer ctx e1, infer ctx e2 with
		       | FLOAT, FLOAT -> FLOAT
		       | INT, INT -> INT
		       | _ , _ -> raise (Error "Ill-Typed!"))
    | Eq(e1, e2) -> let t = infer ctx e1 in
		    if t = (infer ctx e2) then BOOL else raise (Error "Ill-Typed")
    | Lt(e1,e2) -> (match infer ctx e1, infer ctx e2 with
		    | INT, INT -> BOOL
		    | FLOAT, FLOAT -> BOOL
		    | _, _ -> raise (Error "Ill-Typed")
    | And(e1,e2) -> if (infer ctx e1 = BOOL && infer ctx e2 = BOOL) then BOOL
		    else raise (Error "Ill-Typed")
    | Or(e1,e2) ->  if (infer ctx e1 = BOOL && infer ctx e2 = BOOL) then BOOL
		    else raise (Error "Ill-Typed")
    | Not(e1) -> if infer ctx e1 = BOOL then BOOL else raise (Error "Ill-Typed")
    | Trunc(e1) -> if infer ctx e1 = FLOAT then INT else raise (Error "Ill-Typed")
    | ToFloat(e1) -> if infer ctx e1 = INT then FLOAT else raise (Error "Ill-Typed") 
    | Int n -> INT 
    | Float n -> FLOAT
    | Bool p -> BOOL
    | If_Then_Else(e1,e2,e3) -> let t = infer ctx e2 in
				if (infer ctx e1 = BOOL && t = infer ctx e3) then t
						    else raise (Error "Ill-Typed")
    | Sum(e1,e2,(y,e3)) -> (match infer ctx e1, infer ctx e2, infer ((y, INT) :: ctx) e3 with
			    | INT, INT , FLOAT -> FLOAT
			    | INT, INT, INT -> INT
			    | _ , _  , _ -> raise (Error "Ill-Typed"))
 
    | Var y ->  contains1 y ctx
		  
  let infer e = infer [] e
		      
 end;;

 module Test = 
 struct
   open ArithExp

   (* (3.0 * 2.0) + ((10.0 * 5)  / 0) *)
   let e1 = Plus(Times(Float 3.0, Float 2.0), 
		 Div(Times(Float 10.0, ToFloat(Int 5)),
   ToFloat (Int 0)))

   (* 3*2 *)
   let e2 = Plus(Times(Float 3.0, Int 2), 
		 Div(Times(ToFloat (Int 10), Int 5),
		     Times(Float 3.2, Float 2.2)))

   (* Example of encoding the sum over x * x for x = 0 to 10 *)
   let e3 = Sum(Int 0, Int 10, ("x", Times(ToFloat (Var "x")  , 
					   ToFloat (Var "x"))))

 end 

