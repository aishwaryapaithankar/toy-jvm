let make_concated_string str values =
  let parts = String.split_on_char '\001' str in
  let rec helper acc parts values =
    match parts, values with
    | [], [] -> acc
    | hd::tl, [] -> acc ^ hd
    | hd::tl, v::vs -> helper (acc ^ hd ^ v) tl vs
    | _ -> failwith "Number of placeholders mismatch"
  in helper "" parts values;;
let values = ["3"; "yo";];;
let str = "\001 is in grade \001";;
let new_str = make_concated_string str values;;
print_endline new_str;;
  
    
  