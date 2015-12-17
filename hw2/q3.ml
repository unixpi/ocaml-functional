let rec iter_sum f (lo, hi) inc =
  let rec sum' (lo, r) = 
    if (lo > hi) then r
    else 
      sum' (inc(lo), r +. f lo)
  in 
    sum' (lo, 0.0);;


let rec conv2 f g dy =                                                                             
    (fun x -> iter_sum (fun y -> f y *. g (x +. y)) (dy /. 2.0, x) (fun y -> y +. dy));;

  (* example *)
conv2 (fun x -> x *. x) (fun y -> 1.0) 0.0001;;

