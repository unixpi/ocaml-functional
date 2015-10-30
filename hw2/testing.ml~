HW2 Q2

let rec sort = function
    | [] -> []
    | a :: rest -> insert a (sort rest)
  and insert element = function
    | [] -> [element]
    | a :: rest -> if element < a then element :: a :: rest
                else a :: insert element rest;;

let eq a b =
     a = b ;;

let rec remove_duplicates eq list =
  match sort list with
  | [] -> []                                                                                       
  | a :: [] -> [a]
  | a :: b :: rest -> if eq a b then remove_duplicates eq (a :: rest)
                               else a :: remove_duplicates eq (b :: rest);;

remove_duplicates (function x -> function y -> x = y) [1;2;2;2];;

remove_duplicates (function x -> function y -> x = y) [1;2;1;2;1];;


HW2 Q3 Convolution

let rec conv f g dy =                                                                                                         
    (fun x -> iter_sum (fun y -> f y *. g (x +. y)) (dy /. 2.0, x) (fun y -> y +. dy));;

conv (fun x -> x *. x) (fun y -> 1.0) 0.01 5.0;;


HW2 Q4 Functional Parsing


