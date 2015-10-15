(* finding the last element of a list *)

let rec last = function
  | [] -> None
  | a :: [] -> a
  | a :: rest -> last rest ;;

let rec last1 l =
  match l with
    | [] -> None
    | a :: [] -> a
    | a :: rest -> last rest ;;

last [ "a" ; "b" ; "c" ; "d" ];;
last1 [ "a" ; "b" ; "c" ; "d" ];;
last [];;
last1 [];;    

(* finding the k'th element of a list *)

let rec at n list  =
  match n, list with
  | 1, a :: _  -> a 
  | _, a :: rest -> at n-1 rest ;;
  
let rec at1 k = function
  | [] -> None
  | h :: t -> if k = 1 then Some h else at (k-1) t;;

  (* find the number of elements of a list *)

let  length list =
  let rec length1 list1 acc =
    match list1 with
    | [] -> acc
    | a :: rest -> length1 rest (acc + 1)  
  in length1 list 0;;

let length2 list =
  let rec aux n = function
    | [] -> n
    | _::t -> aux (n+1) t
   in aux 0 list;;
    
length [ "a" ; "b" ; "c"];;
length2 [ "a" ; "b" ; "c"];;    
  
  (* fibonnaci using pattern matching *)

let rec fib n =
  match n with
  | 0 -> 0
  | 1 -> 1	   
  | _ -> fib (n-1) + fib (n-2);;

let rec fib1 = function
    0 -> 0
  | 1 -> 1
  | i -> fib1 (i-1) + fib1 (i-2);;

let is_uppercase = function
    'A' .. 'Z' -> true
  | _ -> false;;

  (* drop all occurences of a given value from a list using pattern matching *)


let drop_value l value =
  let rec aux l value l1 =
    match l with
      | [] -> l1 
      | a :: r -> if a = value then aux r value l1 else aux r value (a :: l1)
  in aux l value [];;

  drop_value [1;2;3;2;2;2;1;2] 2;;

  let rec drop_value1 l to_drop =
    match l with
    | [] -> []
    | hd :: tl ->
       let new_tl = drop_value1 tl to_drop in
       if hd = to_drop then new_tl else hd :: new_tl;;

  let rec drop_zero l =
    match l with
    | [] -> []
    | 0  :: tl -> drop_zero tl
    | hd :: tl -> hd :: drop_zero tl;;

    drop_value [1;2;3] 2;;

      (* re implementing List.filter *)

   let rec filter l f =
      match l with
        | [] -> []
        | hd :: tail -> if f hd then hd :: (filter tail f) else filter tail f;;

      filter [1;2;3;4;5] (fun x -> x mod 2 = 0);;

	(* re implementing List.map *)

      let rec map l f =
	match l with
	  | [] -> []
          | hd :: tail -> f hd :: (map tail f) ;;

    	(* filter and map *)

      let rec filter_map l f g =
	match l with
	| [] -> []
	| hd :: tail -> if f hd then g hd :: (filter_map tail f g) else
			  filter_map tail f g;;

      let rec filter_map l f g =
	let filtered_list = filter l f in map filtered_list g;;

	(* reduce *)

      let rec reduce f acc list  = (* note we will return a function that
                                        takes a list as input *)
	match list with
       	| [] -> acc
	| hd :: tl -> reduce f (f hd acc) tl ;;  

	reduce (fun x -> fun y -> x + y) 0 [1;2;3;4];;

	reduce (fun x y -> x + y) 0 [1;2;3;4];;

	(* fold *)
	
        let sum = reduce (fun a x -> x + a) 0;;

	  
	sum [1;2;3;4];;

	let concat = reduce (fun a x -> x ^ a) "";;

	let sum = reduce (+) 0;;
	let concat = reduce (^) "";;

        let length = reduce (fun x acc -> acc + 1) 0;;

	let rev = reduce (fun x acc -> x :: acc ) [];;

	  (* writing map in terms of reduce *)
        
	let map f list = reduce (fun x acc -> f x :: acc) [] (rev list);;
	  
	(* note reduce is very similar to fold_left *)
   
        let rec reduce f acc list  = (* note we will return a function that
                                        takes a list as input *)
	  match list with
       	  | [] -> acc
	  | hd :: tl -> reduce f (f hd acc) tl ;;

	let rec fold_left (f : 'a -> 'b -> 'a) (acc : 'a) (l : 'b list) : 'a =
	  match l with
	    [] -> acc
	  | h :: t -> fold_left f (f acc h) t;;

	let rec fold_right (f : 'a -> 'b -> 'b) (l : 'a list) (acc : 'b) : 'b =
	  match l with
	    [] -> acc
	  | h :: t -> f h (fold_right f t acc);;

	let sum = fold_left (fun acc x -> acc + x) 0;;

	let rev = fold_left (fun acc x -> x :: acc) [];;

	let length = fold_left (fun acc x -> acc + 1) 0;;

	let map f l = fold_right (fun x acc -> f x :: acc) l [];;

	let filter f l = fold_right (fun x acc -> if f x
						 then x :: acc
						 else acc) l [];;

	(* implementing fold right in terms of fold left *)

	let fold_right_2 f l acc = fold_left (fun acc x -> f x acc) acc (List.rev l);; 
type exp =
  | PLUS of exp * exp
  | MINUS of exp * exp
  | TIMES of exp * exp
  | DIV of exp * exp
  | SIN of exp
  | COS of exp
  | EXP of exp * exp
  | FLOAT of float ;;

let rec eval e = match e with
  | PLUS (e1,e2) -> (eval e1) +. (eval e2)
  | MINUS (e1,e2) -> (eval e1) -. (eval e2)
  | TIMES (e1,e2) -> (eval e1) *. (eval e2)
  | DIV (e1,e2) -> (eval e1) /. (eval e2)
  | SIN e -> sin (eval e)
  | COS e -> cos (eval e)
  | EXP (e1,e2) -> (eval e1) ** (eval e2)
  | FLOAT e -> e;;
(* eval (TIMES (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0));;
- : float = 27.5 *)
type instruction =
  Plus | Minus | Times | Div | Sin | Cos | Exp | Float of float ;;
  
let to_instr  e =
  let rec aux e list =
    match e with
    | PLUS (e1,e2) -> aux e1 list @ aux e2 (Plus :: list)
    | MINUS (e1,e2) -> aux e1 list @ aux e2 (Minus :: list)
    | TIMES (e1,e2) -> aux e1 list @ aux e2 (Times :: list)
    | DIV (e1,e2) -> aux e1 list @ aux e2 (Div :: list)
    | SIN e -> aux e (Sin :: list)
    | COS e -> aux e (Cos :: list)
    | EXP (e1,e2) -> aux e1 list @ aux e2 (Exp :: list)
    | FLOAT e -> Float e :: list
  in aux e [];; 

to_instr (TIMES (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0));;
  (* result is [Float 2.2; Float 3.3; Plus; Float 5.; Times] *)				      
    
	  
	  
