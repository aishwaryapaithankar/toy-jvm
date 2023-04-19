#use "class_file_parse.ml";;

let class_file = parse_file  "test/Factorial.class";;

let rec find_method (l:field list) mn= match l with
| [] -> raise (Failure "method not found")
| x :: xs -> if x.fNameIndex = mn then x else find_method xs mn;;

let rec find_code_attribute (l: attr list) (cp:const list) = match l with
| [] -> raise (Failure "code attribute not found")
| x :: xs -> if (resolve cp x.aNameIndex) = "Code" then x else find_code_attribute xs cp;;

let create_frame (mn:int) c (args : var list) = 
    let m = find_method c.methods mn in
    let ca = find_code_attribute m.attrInfo c.constPool in
    let (max_stack,cb) = split_n 2 ca.info in 
    let (max_local,cb) = split_n 2 cb in
    let _ =  fst (parse_two_bytes max_stack) in
    let max_local = fst (parse_two_bytes max_local) in
    let cb_len,cb = parse_four_bytes cb in
    let (cb,_) = split_n cb_len cb in
    let (l: var array) = Array.make max_local (Obj.magic ()) in
    List.iteri (fun i arg -> match arg with
      | Int x -> if i < max_local then l.(i) <- Int x else ()
      | Str x -> if i < max_local then l.(i) <- Str x else ()) args;
   {class_file= c;method_name= (resolve c.constPool m.fNameIndex);ip= 0;code= cb;locals= l;stack= (Stack.empty_stack);}
  ;;

let update_frame (f:jvmframe) (s:Stack.t) = {f with stack=s;ip=f.ip+1}

let update_frame_inc_ip (f:jvmframe) (s:Stack.t) (i:int)= {f with stack=s;ip=f.ip+i}

let update_frame_set_ip (f:jvmframe) (s:Stack.t) (i:int)= {f with stack=s;ip=i}

let get_name_and_args (cp:const list) (i:int) =  
let i = (List.nth cp (i-1)).nameAndTypeIndex in
let nti = List.nth cp (i-1) in
((nti.cNameIndex),(List.nth cp (nti.descIndex -1)).cString);; (*find method based on fst index and snd is the descripter string to be parsed. *)

let parse_descriptor str = 
  let n = String.length str in
  let rec aux (i:int) (a:descriptor list) =
    if i = n then List.rev a
    else match (str.[i]) with
    | '(' | ')' ->  aux (i+1) a
    | 'I' ->  aux (i+1) (I::a)
    | 'F' ->  aux (i+1) (F::a)
    | _ -> failwith "invalid descriptor"
  in aux 0 [];;

let create_args (li:descriptor list) (stack:Stack.t) = 
  let rec aux l s a = 
    match l with
    | [x] -> (s, a) (*Leave last element as it is as last element is return type*)
    | x::xs ->  let a' = (Stack.peek s :: a) in aux xs (Stack.pop s) a' 
    | [] -> failwith "Accidently passed return value in args"
  in aux li stack [];;

let rec exec (f: jvmframe) =
  (* let _ = read_line() in *)
  let op = (List.nth f.code f.ip) in
  let _ = Stack.print_stack (f.method_name) op (f.stack)  in
  match op with 
  | 2 | 3 | 4 | 5 | 6 | 7 | 8 (*iconst*)  -> exec @@ update_frame f @@ Stack.push (Int(op-3)) f.stack
  | 16 (*bipush*) -> let v = (List.nth f.code (f.ip+1)) in exec @@ update_frame_inc_ip f (Stack.push (Int(v)) (f.stack)) 2
  | 26 | 27 | 28 | 29 (* iload *) -> (match f.locals.(op-26) with | Int(_) as x -> exec @@ update_frame f @@ Stack.push x f.stack
                                                 | _ -> failwith "Required int found something else") 
  | 54 (*istore*) ->  let i = (List.nth f.code (f.ip+1)) in 
                      let x = (Stack.peek f.stack) in
                      f.locals.(i) <- x;
                      exec @@ update_frame_inc_ip f (Stack.pop f.stack) 2
  | 59 | 60 | 61 | 62 (* istore_ *) -> let Int(x) = (Stack.peek f.stack)  in f.locals.(op-59) <- Int(x);
                                       exec @@ update_frame f @@ Stack.pop f.stack
  | 87 (*pop*) ->  exec @@ update_frame f @@ Stack.pop f.stack           
  | 96 (*iadd*) ->  exec @@ update_frame f @@ Stack.i_add f.stack         
  | 100 (*isub*) -> exec @@ update_frame f @@ Stack.i_sub f.stack 
  | 104 (*imul*) -> exec @@ update_frame f @@ Stack.i_mul f.stack     
  | 108 (*idiv*) -> exec @@ update_frame f @@ Stack.i_div f.stack                       
  | 160 (*if_icmpne*) ->  let (b,s') = Stack.if_icmpne f.stack in
                          if b then 
                          let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in 
                          let _ = print_endline (string_of_int (i)) in
                          exec @@ update_frame_set_ip f s' (f.ip+i)
                          else exec @@ update_frame_inc_ip f s' 3
                      
  | 172 (*ireturn*) -> Stack.peek f.stack (*Caller should push this value to the stack*)
  | 177 ->  Void   
  | 184 (*invokestatic*) ->  let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in 
                             let (mi, d) =  get_name_and_args f.class_file.constPool i in
                             let (new_stack,args) = create_args (parse_descriptor d) f.stack in
                             let new_frame = create_frame mi (f.class_file) args in
                             let return_val = exec new_frame in (*change this to check void type*)
                            exec @@ update_frame_inc_ip f  (Stack.push return_val new_stack) (3)
  | _ -> print_endline (string_of_int op);failwith "OOPS";;  


let find_main c = 
    let rec aux i = 
      let index = (List.nth c.methods i).fNameIndex in 
      match resolve c.constPool index  with 
      | "main" -> index 
      | _ -> aux (i+1) 
    in aux 0;;

let main_index = find_main class_file in 
let main_frame = create_frame main_index class_file [] in  (*assumed that main will have no args*)
exec main_frame;;
(* (main_frame.class_file.methods);; *)

(* {accessFlag = 9; fNameIndex = 11; descIndex = 12; attrCount = 1;
  attrInfo =
   [{aNameIndex = 13; len = 53;
     info =
      [0; 3; 0; 1; 0; 0; 0; 16; 26; 4; 160; 0; 5; 4; 172; 26; 26; 4; 100;
       184; 0; 7; 104; 172; 0; 0; 0; 2; 0; 14; 0; 0; 0; 10; 0; 2; 0; 0; 0; 3;
       0; 7; 0; 4; 0; 15; 0; 0; 0; 3; 0; 1; 7]}]};
 {accessFlag = 9; fNameIndex = 16; descIndex = 17; attrCount = 1;
  attrInfo =
   [{aNameIndex = 13; len = 34;
     info =
      [0; 1; 0; 1; 0; 0; 0; 6; 7; 184; 0; 7; 87; 177; 0; 0; 0; 1; 0; 14; 0;
       0; 0; 10; 0; 2; 0; 0; 0; 7; 0; 5; 0; 8]}]}]*)
