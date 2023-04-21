#use "class_file_parse.ml";;

let class_file = parse_file  "test/Inheritance.class";;

let rec find_method (l:field list) mn= match l with
| [] -> raise (Failure "method not found")
| x :: xs -> if x.fNameIndex = mn then x else find_method xs mn;;

(* let rec find_method_by_name (l:field list) c mn = match l with
| [] -> raise (Failure "method not found")
| x :: xs -> if (resolve c x.fNameIndex) = mn then x else find_method xs mn;; *)

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
      | Str x -> if i < max_local then l.(i) <- Str x else ()
      | CRef x -> if i < max_local then l.(i) <- CRef x else ()) args;
   {class_file= c;method_name= (resolve c.constPool m.fNameIndex);ip= 0;code= cb;locals= l;stack= (Stack.empty_stack);}
  ;;

let update_frame (f:jvmframe) (s:Stack.t) = {f with stack=s;ip=f.ip+1}

let update_frame_inc_ip (f:jvmframe) (s:Stack.t) (i:int)= {f with stack=s;ip=f.ip+i}

let update_frame_set_ip (f:jvmframe) (s:Stack.t) (i:int)= {f with stack=s;ip=i}

let get_name_and_args (cp:const list) (i:int) =  
  let i = (List.nth cp (i-1)).nameAndTypeIndex in
  let nti = List.nth cp (i-1) in
  ((nti.cNameIndex),(List.nth cp (nti.descIndex -1)).cString);; (*find method based on fst index and snd is the descripter string to be parsed. *)


let jobj_method (cp:const list) (i:int) =   
  let i = (List.nth cp (i-1)).classIndex in 
  let ci = (List.nth cp (i-1)).cNameIndex in 
  if resolve cp ci = "java/lang/Object" then true else false


let parse_descriptor str = 
  let n = String.length str in
  let rec aux (i:int) (a:descriptor list) =
    if i = n then List.rev a
    else match (str.[i]) with
    | '(' | ')' ->  aux (i+1) a
    | 'I' ->  aux (i+1) (I::a)
    | 'F' ->  aux (i+1) (F::a)
    | 'Z' ->  aux (i+1) (Z::a)
    | 'L' -> let j = String.index_from str (i+1) ';' in
             let class_name = String.sub str (i+1) (j-i-1) in
             let _ = print_endline class_name in 
             aux (j+1) (S::a)
    | 'V' -> aux (i+1) (V::a)
    | _ -> failwith "invalid descriptor " 
  in aux 0 [];;

let create_args (li:descriptor list) (stack:Stack.t) = 
  let rec aux l s a = 
    match l with
    | [x] -> (s, a) (*Leave last element as it is as last element is return type*)
    | x::xs ->  let a' = (Stack.peek s :: a) in aux xs (Stack.pop s) a' 
    | [] -> failwith "Accidently passed return value in args"
  in aux li stack [];;

let create_args_with_obj_ref  (li:descriptor list) (stack:Stack.t) = 
    let rec aux l s a = 
      match l with
      | [x] ->  (s, (Stack.peek s :: a)) (*Leave last element as it is as last element is return type*)
      | x::xs ->  let a' = (Stack.peek s :: a) in aux xs (Stack.pop s) a' 
      | [] -> failwith "Accidently passed return value in args"
    in aux li stack [];;

let signed_byte x = if x > 127 then -(((lnot x) land 0xFF) + 1) else x
let signed_two_bytes x = if (x land 0x8000 > 0) then -(((lnot x) land 0xFFFF) + 1) else x

let create_obj (clsname:string) : (cls* var field_list list ref) = 
  let obj_classfile = parse_file @@ "test/"^clsname^".class" in
  let c = obj_classfile.constPool in 
  let fl = 
    (let rec aux l a = 
        match l with
        | x::xs -> (match (resolve c (x.descIndex) ) with 
                    | "I" -> aux xs (({name=(resolve c (x.fNameIndex)); value=Int(0)})::a)
                    | "Ljava/lang/String;" -> aux xs (({name=(resolve c (x.fNameIndex)); value=Str("")})::a)
                    | _ -> failwith "Unknown DataType found while creating new obj")
        | [] -> a 
      in aux obj_classfile.fields []) 
  in (obj_classfile,ref fl);;

let update_obj_ref name value obj_ref =
  obj_ref := List.map (fun field ->
    if field.name = name then {field with value = value} else field
  ) !obj_ref
  
  
let rec exec (f: jvmframe) =
  (* let _ = read_line() in *)
  let op = (List.nth f.code f.ip) in
  let _ = Stack.print_stack (f.class_file.name) (f.method_name) op (f.stack)  in
  match op with 
  | 2 | 3 | 4 | 5 | 6 | 7 | 8 (*iconst*)  -> exec @@ update_frame f @@ Stack.push (Int(op-3)) f.stack
  | 16 (*bipush*) -> let v = (List.nth f.code (f.ip+1)) in exec @@ update_frame_inc_ip f (Stack.push (Int(v)) (f.stack)) 2
  | 18 (*ldc*) -> let i = (resolve f.class_file.constPool (List.nth f.code (f.ip+1)))
                          in exec @@ update_frame_inc_ip f (Stack.push (Str(i)) (f.stack)) 2 (*TODO: Handle int, classref here*)
  | 26 | 27 | 28 | 29 (* iload *) -> (match f.locals.(op-26) with | Int(_) as x -> exec @@ update_frame f @@ Stack.push x f.stack
                                                 | _ -> failwith "Required int found something else in iload") 
  | 42 | 43 | 44 | 45 (*aload*) -> exec @@ update_frame f @@ Stack.push (f.locals.(op-42)) f.stack
  | 54 (*istore*) ->  let i = (List.nth f.code (f.ip+1)) in 
                      let x = (Stack.peek f.stack) in
                      f.locals.(i) <- x;
                      exec @@ update_frame_inc_ip f (Stack.pop f.stack) 2
  | 59 | 60 | 61 | 62 (*istore*) -> let Int(x) = (Stack.peek f.stack)  in f.locals.(op-59) <- Int(x);
                                       exec @@ update_frame f @@ Stack.pop f.stack
  | 75 | 76 | 77 | 78 (*astore*) -> let x = (Stack.peek f.stack) in
                                    f.locals.(op-75) <- x;
                                    exec @@ update_frame f (Stack.pop f.stack)
  | 87 (*pop*) ->  exec @@ update_frame f @@ Stack.pop f.stack           
  | 89 (*dup*) ->  exec @@ update_frame f @@ (Stack.push (Stack.peek f.stack) f.stack)
  | 96 (*iadd*) ->  exec @@ update_frame f @@ Stack.i_add f.stack         
  | 100 (*isub*) -> exec @@ update_frame f @@ Stack.i_sub f.stack 
  | 104 (*imul*) -> exec @@ update_frame f @@ Stack.i_mul f.stack     
  | 108 (*idiv*) -> exec @@ update_frame f @@ Stack.i_div f.stack
  | 132 (*iinc*) ->  let i = (List.nth f.code (f.ip+1)) in
                 let c = signed_byte (List.nth f.code (f.ip+2)) in
                 let Int(x) = f.locals.(i) in
                 f.locals.(i) <- (Int(x + c));
                 exec @@ update_frame_inc_ip f f.stack 3
  | 153 (*ifeq*) ->  let (b,s') = Stack.ifeq f.stack in
                    if b then 
                    let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in 
                    let _ = print_endline (string_of_int (i)) in
                    exec @@ update_frame_set_ip f s' (f.ip+i)
                    else exec @@ update_frame_inc_ip f s' 3 
  | 158 (*ifle*) ->   let (b,s') = Stack.ifle f.stack in
                      if b then 
                      let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in 
                      let _ = print_endline (string_of_int (i)) in
                      exec @@ update_frame_set_ip f s' (f.ip+i)
                      else exec @@ update_frame_inc_ip f s' 3                     
  | 160 (*if_icmpne*) ->  let (b,s') = Stack.if_icmpne f.stack in
                          if b then 
                          let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in 
                          let _ = print_endline (string_of_int (i)) in
                          exec @@ update_frame_set_ip f s' (f.ip+i)
                          else exec @@ update_frame_inc_ip f s' 3
  | 167 (*goto*) -> let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in
                    let i = signed_two_bytes i in
                    exec @@ update_frame_set_ip f f.stack (f.ip+i)
  | 172 (*ireturn*) -> Stack.peek f.stack (*Caller should push this value to the stack*)
  | 177 ->  Void  
  | 181 (*putfield*) -> let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in
                        let (fi, d) =  get_name_and_args f.class_file.constPool i in
                        let fn = resolve f.class_file.constPool fi in
                        let v = Stack.peek f.stack in 
                        let new_stack = Stack.pop f.stack in 
                        let CRef(obj) = Stack.peek new_stack in
                        let new_stack = Stack.pop new_stack in
                        let obj = snd obj in
                        update_obj_ref fn v obj; 
                        exec @@ update_frame_inc_ip f new_stack 3
  | 182 (*invokevirtual*) -> let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in 
                            let (mi, d) =  get_name_and_args f.class_file.constPool i in
                            let _ = print_endline ("found MI as " ^ (resolve f.class_file.constPool (mi))) in
                            let (new_stack,args) = create_args_with_obj_ref (parse_descriptor d) f.stack in
                            let CRef(obj) = List.nth args 0 in
                            let obj_cls = (fst obj) in
                            CRef(obj)
  | 183 (*invokespecial*)-> let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in 
                            if (jobj_method f.class_file.constPool i) then (exec @@ update_frame_inc_ip f  (Stack.pop  f.stack) (3)) else
                              let (mi, d) =  get_name_and_args f.class_file.constPool i in
                              let _ = print_endline ("found desc as " ^ (d)) in
                              let (new_stack,args) = create_args_with_obj_ref (parse_descriptor d) f.stack in
                              let CRef(obj) = List.nth args 0 in
                              let obj_cls = (fst obj) in
                              let new_frame = create_frame mi obj_cls args in
                              let return_val = exec new_frame in
                              (match return_val with |Void ->  print_endline "found void";exec @@ update_frame_inc_ip f new_stack 3 | _ ->  print_endline "found rv";exec @@ update_frame_inc_ip f  (Stack.push return_val new_stack) (3))
  | 184 (*invokestatic*) ->  let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in 
                             let (mi, d) =  get_name_and_args f.class_file.constPool i in
                             let (new_stack,args) = create_args (parse_descriptor d) f.stack in
                             let new_frame = create_frame mi (f.class_file) args in
                             (* let return_val = exec new_frame in (*change this to check void type*)
                             exec @@ update_frame_inc_ip f  (Stack.push return_val new_stack) (3) *)
                             let return_val = exec new_frame in
                            (match return_val with |Void ->  exec @@ update_frame_inc_ip f new_stack 3 | _ ->  exec @@ update_frame_inc_ip f  (Stack.push return_val new_stack) (3))
  | 187 (*new*) -> let i = (List.nth f.code (f.ip+1)) * 256 + (List.nth f.code (f.ip+2)) in 
                   let obj = CRef(create_obj (resolve f.class_file.constPool i)) in
                   exec @@ update_frame_inc_ip f (Stack.push obj f.stack) (3)    
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
(* let o = create_obj "Person" in (fst o,!(snd o));;
; *)
(* (main_frame);; *)
(* parse_descriptor "(Ljava/lang/String;I)V";; *)

(*   methods =
    [{accessFlag = 1; fNameIndex = 5; descIndex = 6; attrCount = 1;
      attrInfo =
       [{aNameIndex = 20; len = 29;
         info =
          [0; 1; 0; 1; 0; 0; 0; 5; 42; 183; 0; 1; 177; 0; 0; 0; 1; 0; 21; 0;
           0; 0; 6; 0; 1; 0; 0; 0; 52]}]};
     {accessFlag = 9; fNameIndex = 22; descIndex = 23; attrCount = 1;
      attrInfo =
       [{aNameIndex = 20; len = 50;
         info =
          [0; 4; 0; 2; 0; 0; 0; 18; 187; 0; 7; 89; 18; 9; 16; 16; 183; 0; 11;
           76; 43; 182; 0; 14; 87; 177; 0; 0; 0; 1; 0; 21; 0; 0; 0; 14; 0; 3;
           0; 0; 0; 54; ...]};
        ...]};
     ...];
   attributes = ...};
 method_name = ...; ip = ...; code = ...; locals = ...; stack = ...}*)