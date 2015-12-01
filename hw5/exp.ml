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

    

  let rec eval (e : exp) : exp = raise (Error "Not Implemented - Your Task!")


				     
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
    | Lt(e1,e2) ->  let t = infer ctx e1 in
		    if t = (infer ctx e2) then BOOL else raise (Error "Ill-Typed")
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
    | Sum(e1,e2,(y,e3)) -> (match infer ctx e1, infer ctx e2 with
			    | INT, INT -> 
			    | _ , _ -> raise (Error "Ill-Typed")
 
    | Var y ->  
		  
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

