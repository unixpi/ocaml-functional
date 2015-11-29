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

  let rec fv (e : exp) : var list = match e with
    | Plus(e1, e2) -> (fv e1) :: (fv e2) 
    | Minus(e1, e2) -> (fv e1) :: (fv e2)
    | Times(e1, e2) -> (fv e1) :: (fv e2)
    | Div(e1, e2) ->  (fv e1) :: (fv e2)
    | Eq(e1, e2) -> (fv e1) :: (fv e2)
    | Lt(e1,e2) -> (fv e1) :: (fv e2)
    | And(e1,e2) -> (fv e1) :: (fv e2)
    | Or(e1,e2) -> (fv e1) :: (fv e2)
    | Not(e1) -> (fv e1)
    | Trunc(e1) -> (fv e1)
    | ToFloat(e1) -> (fv el)
    | Int n -> []
    | Float n -> []
    | Bool p -> []
    | If_then_Else(e1,e2,e3) -> (fv e1) :: (fv e2) :: (fv e3)
    | Sum(e1,e2,(x,e3)) -> (fv e1) :: (fv e2) :: (fv e3) :: [x]
    | Var x -> [x]


  let rec subst (e,x : exp * var) (e' : exp) : exp = raise (Error "Not Implemented - Your Task!")

  let rec eval (e : exp) : exp = raise (Error "Not Implemented - Your Task!")

  let rec infer (ctx : (var * tp) list) (e : exp) : tp = raise (Error "Not Implemented - Your Task!")

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

