#use "class_file_parse.ml";;

let class_file = parse_file  "Add.class";;

let rec find_method (l:field list) mn= match l with
| [] -> raise (Failure "method not found")
| x :: xs -> if x.fNameIndex = mn then x else find_method xs mn;;

let rec find_code_attribute (l: attr list) (cp:const list) = match l with
| [] -> raise (Failure "code attribute not found")
| x :: xs -> if (resolve cp x.aNameIndex) = "Code" then x else find_code_attribute xs cp;;


let create_frame (mn:int) c (args : var list) = 
    let m = find_method c.methods 11 in
    let ca = find_code_attribute m.attrInfo c.constPool in
    let (max_stack,cb) = split_n 2 ca.info in 
    let (max_local,cb) = split_n 2 cb in
    let max_stack =  fst (parse_two_bytes max_stack) in
    let max_local = fst (parse_two_bytes max_local) in
    let cb_len,cb = parse_four_bytes cb in
    let (cb,_) = split_n cb_len cb in
    let (l: var array) = Array.make max_local (Obj.magic ()) in
    List.iteri (fun i arg -> match arg with
      | Int x -> if i < max_local then l.(i) <- Int x else ()
      | Str x -> if i < max_local then l.(i) <- Str x else ()) args;
   {class_file= c;ip= 0;code= cb;locals= l;stack= empty_stack;}
  ;;

let frame = create_frame 11 class_file [ Int(1); Str("ab") ];;


let rec exec f  =
  let op = (List.nth f.code f.ip) in
  match op with 
  | 26 (* iload_0 *) -> (match f.locals.(0) with | Int(x) -> let new_frame = {f with stack=(push x f.stack);ip=(f.ip+1)} in exec new_frame
                                                 | _ -> failwith "Required int found something else")
  | 27 (* iload_1 *) -> (match f.locals.(1) with | Int(x) ->{f with stack=(push x f.stack)} 
                                                 | _ -> failwith "Required int found something else")                                          
  | _ -> failwith "OOPS";;  
    (* let op = List.nth frame.code frame.ip;; in
    match op with
    | 26 (* iload_0 *) -> Str("")
    | _ -> failwith "OOPS";;  *)
;;

exec frame;;