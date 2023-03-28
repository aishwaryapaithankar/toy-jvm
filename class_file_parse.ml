 
let parse_eight_bytes list_bytes =
  let rec aux acc n = function
    | [] -> if n = 8 then (acc,[]) else failwith "parse_eight_bytes failed"
    | x::xs -> if n = 8 then (acc,x::xs) else
        let shifted_byte = (Int64.shift_left ( acc) 8) in
        aux (Int64.add shifted_byte (Int64.of_int x)) (n+1) xs
  in 
  aux 0L 0 list_bytes;;


let parse_four_bytes list_bytes = match list_bytes with
| a::b::c::d::xs -> let i =  ((Int.shift_left (a) 24) lor
                    (Int.shift_left (b) 16) lor
                    (Int.shift_left (c) 8) lor 
                    (d)) in
                    (i,xs)
| _ -> failwith "parse_four_bytes failed";;


let parse_two_bytes list_bytes = match list_bytes with
| a::b::xs -> let i =  (Int.shift_left (a) 8) lor b in
                    (i,xs)
| _ -> failwith "parse_two_bytes failed";; 


let parse_magicword list_bytes = match list_bytes with
 | 202::254::186::190::xs -> (xs)
 | _ -> failwith "parse_magicword failed";;

let parse_version list_bytes = match list_bytes with
 | a::b::c::d::xs -> (xs)
 | _ -> failwith "parse_version failed";;

let parse_constant_pool_count list_bytes = match list_bytes with
 | a::b::xs -> ((a*256+b),xs)
 | _ -> failwith "parse_constant_pool_count failed";;

let get_bytes fn =
  let inc = open_in_bin fn in
  let rec go sofar =
      match input_char inc with
      | b -> go (Char.code b :: sofar)
      | exception End_of_file -> List.rev sofar
  in
  let res = go [] in
  close_in inc;
  res ;;

type const = 
  | Tag of int
  | NameIndex of int
  | ClassIndex of int
  | NameAndTypeIndex of int
  | StringIndex of int
  | DescIndex of int
  | String of string

let parse_constant_pool list_bytes count = 
  let rec parse_constant_pool_helper l c = match list_bytes with
    | 7::xs (* CONSTANT_Class  *) -> failwith "notimpl"
    | 9::xs (* CONSTANT_Fieldref  *) -> failwith "notimpl"
    | 10::xs (* CONSTANT_Methodref  *) -> failwith "notimpl"
    | 11::xs (* CONSTANT_InterfaceMethodref  *) -> failwith "notimpl"
    | 8::xs (* CONSTANT_String  *) -> failwith "notimpl"
    | 3::xs (* CONSTANT_Integer  *) -> failwith "notimpl"
    | 4::xs (* CONSTANT_Float  *) -> failwith "notimpl"
    | 5::xs (* CONSTANT_Long  *) -> failwith "notimpl"
    | 6::xs (* CONSTANT_Double  *) -> failwith "notimpl"
    | 12::xs (* CONSTANT_NameAndType  *) -> failwith "notimpl"
    | 1::xs (* CONSTANT_Utf8  *) -> failwith "notimpl"
    | 15::xs (* CONSTANT_MethodHandle  *) -> failwith "notimpl"
    | 16::xs (* CONSTANT_MethodType  *) -> failwith "notimpl"
    | 17::xs (* CONSTANT_Dynamic  *) -> failwith "notimpl"
    | 18::xs (* CONSTANT_InvokeDynamic  *) -> failwith "notimpl"
    | 19::xs (* CONSTANT_Module  *) -> failwith "notimpl"
    | 20::xs (* CONSTANT_Package  *) -> failwith "notimpl" in
    parse_constant_pool_helper list_bytes count;; 

let b = get_bytes "Add.class" in 
let b' = parse_magicword b in
let bytes_after_version_parsing = parse_version b' in
let (constant_pool_count, bytes_after_parse_constant_pool_count) = parse_constant_pool_count bytes_after_version_parsing in
constant_pool_count
;;  

parse_eight_bytes [127;255;255;255;255;255;255;255] ;;



