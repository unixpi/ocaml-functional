let rec append_tr lista listb =
  let rec append' lista listb cont = match lista with
    | [] -> cont listb
    | h :: t -> append' t listb (fun y -> cont (h :: y)) 
      
  in append' lista listb (fun x -> x);;

(* much simpler to use an accumulator here *)
let rec length_tr l =
  let rec length' l c = match l with
    | [] -> c 1
    | h :: t -> length' t (fun y -> (c h) + 1)

  in length' l (fun x -> 0);;

let rec rev_tr l =
  let rec rev' l acc = match l with
    | [] -> acc
    | h :: t -> rev' t (h :: acc)
  in rev' l [];;

let rec flatten l = match l with
  | [] -> []
  | h :: t -> h @ flatten t;;

let rec flatten_tr l =
  let rec flatten' l c = match l with
    | [] -> c []
    | h :: t -> flatten' t (fun y -> c h @ y)

  in flatten' l (fun x -> x);;

let rec map f l = match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t);;

let rec map_tr f l =
  let rec map' f l c = match l with
    | [] -> c []
    | h :: t -> map' f t (fun y -> c ((f h) :: y))
  in map' f l (fun x -> x);;

let rec filter p l = match l with
  | [] -> []
  | h :: t -> if (p h) then h :: (filter p t)
	      else
		filter p t;;

let rec filter_tr p l =
  let rec filter' p l c = match l with
    | [] -> c []
    | h :: t -> if (p h) then (filter' p t (fun y -> c (h :: y)))
		else
		  filter' p t (fun y -> c y)
  in filter' p l (fun x -> x);;

let rec fold_left f acc l = match l with
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t;;
(* given l = [x1;x2;...xn]  evaluates to f (... (f (f acc x1) x2) ...) xn *)
(* i.e traverses list from left to right *)

let sum l =
  fold_left (+) 0 l;;

let sum' = fold_left (+) 0;;

let concat = fold_left (fun a x -> a ^ x) "";;

let rec fold_right f l acc = match l with
  | [] -> acc
  | h :: t -> f h (fold_right f t acc);;
  (*  f [x1; x2; ...; xn] acc evaluates to f x1 (f x2 (... (f xn acc)...)) *)
  (* i.e. traverses list from right to left *)

let sum'' = (fun list -> fold_right (+) list 0);;

let rec fold_right_tr f l acc =
  let rec fold_right' f l acc cont = match l with
    | [] -> cont acc
    | h :: t -> fold_right' f t acc (fun y -> (cont ((f h) y)))

  in fold_right' f l acc (fun x -> x);;

let sum''' = (fun list -> fold_right_tr (+) list 0);;

  
