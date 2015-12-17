#load "str.cma";;
#use "ordered.ml";;
#use "dict.ml";;

let read_all filename =
  let ch = open_in filename in
  let words = ref [] in
  try
    while true do
      words := input_line ch :: !words
    done;
    !words
  with End_of_file ->
    begin
      close_in ch;
      !words
    end
