
let rec hilo n =
  let () = print_string "type a number: " in
  let i = read_int ()
  in
  if i = n then
    let () = print_string "BRAVO" in
    let () = print_newline ()
    in print_newline ()
  else
    let () =
      if i < n then
	let () = print_string "Higher"
	in print_newline ()
      else
	let () = print_string "Lower"
	in print_newline ()
     in hilo n ;;
 
