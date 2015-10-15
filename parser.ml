
exception NotImplemented 

(* Some string helper functions *)

let to_list (s : string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec of_list (s : char list) : string =
  match s with
    | [] -> ""
    | c :: cs -> String.concat "" [String.make 1 c ; (of_list cs)]

let is_digit (c : char) : bool = match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let is_lower (c : char) : bool =
   (c >= 'a') && (c <= 'z')

let is_upper (c : char) : bool =
   (c >= 'A') && (c <= 'Z')

let is_letter (c : char) : bool =
  (is_upper c) || (is_lower c)

let from_char (c:char) : string  = String.make 1 c

(** FUNCTIONAL PARSERS **)

(* The type of parsers *)

type 'a parser = string -> ('a * string) option

(* Two simple and useful parsers *)

let return v : 'a parser =  fun inp -> Some (v,inp)

let failure : 'a parser = fun inp -> None

(* Still a simple parser *)

let item : char parser =
  fun inp -> let inp' = to_list inp in
	     match inp' with
	       | [] -> None
	       | (x::xs) -> Some (x, of_list xs)

(* Using parsers *)

let parse (p : 'a parser) (inp : string) = p inp

(** Sequencing **)

let (>>) p q =
    fun inp  -> match parse p inp with
      | None -> None
      | Some (_, out) -> parse q out

let (>>=) p f =
    fun inp -> match parse p inp with
      | None -> None
      | Some (v, out) -> parse (f v) out

(* example *)

let p  = item >>= fun x ->
         item >>
         item >>= fun y ->
         return (x,y)


(** Choice **)

let (+++) p q =
    fun inp ->  match parse p inp with
      | None -> parse q inp
      | Some (v,out) -> Some (v, out)

(*
# parse (item +++ return 'd') "abc";;
- : (char * string) option = Some ('a', "bc")

# parse (failure +++ return 'd') "abc";;
- : (char * string) option = Some ('d', "abc")
*)

(** derived primitives **)

let sat : (char -> bool) -> char parser
    = fun p -> item >>= (fun v1 ->
              if p v1 then return v1 else failure)

let digit : char parser
    = sat is_digit

let lower : char parser
    = sat is_lower
let upper : char parser
    = sat is_upper
let letter : char parser
    = sat is_letter
let alphanum : char parser
    = letter +++ digit

let ch : char -> char parser
    = fun c -> sat (fun c' -> c' = c)

(* Another interesting way of combining parsers is by repeatedly
applying a parser, the following two parsers apply as many times as
possible the parser passed as a parameter.

many: applies zero or more times the parser while
many1: requires the parser to at least succeed once.
*)

let rec many : 'a parser -> ('a list) parser
    = fun p -> (many1 p) +++ (return [])

and many1 : 'a parser -> ('a list) parser
    = fun p -> p                >>= fun v1 ->
	      many p            >>= fun v2 ->
	      return (v1 :: v2)

(* A parser that accepts natural numbers *)
let nat = many1 digit >>= fun v1 ->
          return (int_of_string (of_list v1))

(** Arithmetic expressions **)

type expr =
    Num of int
  | Plus of expr * expr
  | Minus of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Exp of expr * expr
  | Sin of expr
  | Cos of expr
  | Neg of expr

(* Q4.1: Factor the grammar considering the precedences:

Expr E := E+E | E-E | E*E | E/E | sin E | cos E | E^E |-E | N
Numbers N := nat
   
   expr::= term1 + expr | term1 - expr | term1
   term1::= term2 * term1 | term2 / term1 | term2
   term2::= term3 ^ term2 | term3
   term3::= -fact | cos fact | sin fact | fact
   fact::= nat 

*)

(* Q4.2: Implement the parser without parenthesis

   Note, we specify explicitly in the function definition the
   input and output types and annotated the input int with the type string and
   declared the return value of this fuction to be of type exp; this is not
   necessary, but clarifies our expectations that the function expr_no_parens
   takes a string as input and returns an expression.
 *)
			    
let rec expr_no_parens inp =
    ((term1 >>= fun v1 ->
    ch '+' >>= fun _ ->
	      expr_no_parens >>= fun v2 ->
	      return (Plus (v1, v2)))
    +++
  (term1 >>= fun v1 ->
   ch '-' >>= fun _ ->
                    expr_no_parens >>= fun v2 ->
                    return (Minus (v1, v2)))
   +++
     term1) inp
and term1 inp =
  ((term2 >>= fun v1 ->
    ch '*' >>= fun _ ->
    term1 >>= fun v2 ->
    return (Mult (v1, v2)))
   +++
     (term2 >>= fun v1 ->
      ch '/' >>= fun _ ->
      term1 >>= fun v2 ->
      return (Div (v1, v2)))
   +++
     term2) inp
and term2 inp =
  ((term3 >>= fun v1 ->
    ch '^' >>= fun _ ->
    term2 >>= fun v2 ->
    return (Exp (v1, v2)))
   +++
     term3) inp
and term3 inp =
  ((ch '-' >>= fun _ ->
   fact >>= fun v2 ->
   return (Neg v2))
+++  
(ch 'c' >>= fun _ ->
 ch 'o' >>= fun _ ->
 ch 's' >>= fun _ ->
 fact >>= fun v2 ->
 return (Cos v2))
+++
(ch 's' >>= fun _ ->
 ch 'i' >>= fun _ ->
 ch 'n' >>= fun _ ->
 fact >>= fun v2 ->
 return (Sin v2))
+++ fact) inp

and fact inp = (nat >>= fun v1 -> return (Num v1)) inp;;

(* Q4.3: Add parenthesis support to your factored grammar

Expr E := E+E | E-E | E*E | E/E | sin E | cos E | E^E |-E | N | (E)
Numbers N := nat

   Hint: don't factor the grammar again, use your answer for Q4.1 as a
   basis and extend it.

   expr::= term1 + expr | term1 - expr | term1
   term1::= term2 * term1 | term2 / term1 | term2
   term2::= term3 ^ term2 | term3
   term3::= -term4 | cos term4 | sin term4 | term4
   term4::= (expr) | fact
   fact::= nat 

 
*)



(* Q4.4: Implement the complete parser by completing the function expr that
   takes a string as input and returns an expression.*)


let rec expr inp =   			    
    ((term1 >>= fun v1 ->
    ch '+' >>= fun _ ->
	      expr >>= fun v2 ->
	      return (Plus (v1, v2)))
    +++
  (term1 >>= fun v1 ->
   ch '-' >>= fun _ ->
                    expr >>= fun v2 ->
                    return (Minus (v1, v2)))
   +++
     term1) inp
and term1 inp =
  ((term2 >>= fun v1 ->
    ch '*' >>= fun _ ->
    term1 >>= fun v2 ->
    return (Mult (v1, v2)))
   +++
     (term2 >>= fun v1 ->
      ch '/' >>= fun _ ->
      term1 >>= fun v2 ->
      return (Div (v1, v2)))
   +++
     term2) inp
and term2 inp =
  ((term3 >>= fun v1 ->
    ch '^' >>= fun _ ->
    term2 >>= fun v2 ->
    return (Exp (v1, v2)))
   +++
     term3) inp
and term3 inp =
  ((ch '-' >>= fun _ ->
   term4 >>= fun v2 ->
   return (Neg v2))
+++  
(ch 'c' >>= fun _ ->
 ch 'o' >>= fun _ ->
 ch 's' >>= fun _ ->
 term4 >>= fun v2 ->
 return (Cos v2))
+++
(ch 's' >>= fun _ ->
 ch 'i' >>= fun _ ->
 ch 'n' >>= fun _ ->
 term4 >>= fun v2 ->
 return (Sin v2))
   +++ term4) inp
and term4 inp =
  ((ch '(' >>= fun _ ->
    expr >>= fun v1 ->
    ch ')' >>= fun _ ->
    return v1)
     +++ fact) inp
	      

and fact inp = (nat >>= fun v1 -> return (Num v1)) inp;;


  (* examples *)

  expr "cos(1+2)^3*4" ;;
  expr_no_parens "1+2*3^4*5";;    
