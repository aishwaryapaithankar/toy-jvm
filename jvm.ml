open Class_file_parse
open Types
open Stack

let (class_list : cl list) = []

let parse_other_class clsname =
  let class_name_matches c name = c.class_name = name in
  let name_exists =
    List.find_opt (fun c -> class_name_matches c clsname) class_list
  in
  match name_exists with
  | Some c -> c.class_file
  | None -> parse_file @@ clsname ^ ".class"

let rec find_method (l : field list) mn =
  match l with
  | [] -> raise (NotFound "method not found while searching by index")
  | x :: xs -> if x.fNameIndex = mn then x else find_method xs mn

let rec find_method_by_name obj_cls mn d =
  let rec aux (l : field list) c =
    match l with
    | [] -> None
    | x :: xs ->
        let (Str tmn) = resolve c x.fNameIndex in
        let (Str td) = resolve c x.descIndex in
        if tmn = mn && td = d then Some (obj_cls, x) else aux xs c
  in
  let found = aux obj_cls.methods obj_cls.constPool in
  match found with
  | Some (obj_cls, x) -> (obj_cls, x)
  | None ->
      if obj_cls.super <> "java/lang/Object" then
        find_method_by_name (parse_other_class obj_cls.super) mn d
      else raise (NotFound "no method found while searching by name")

let rec find_code_attribute (li : attr list) (c : cls) =
  let rec aux (l : attr list) =
    match l with
    | [] -> None
    | x :: xs ->
        let (Str mn) = resolve c.constPool x.aNameIndex in
        if mn = "Code" then Some x else aux xs
  in
  let found = aux li in
  match found with
  | Some x -> x
  | None ->
      if c.super <> "java/lang/Object" then
        find_code_attribute li (parse_other_class c.super)
      else raise (NotFound "code attribute not found")

let create_frame m c (args : var list) =
  let ca = find_code_attribute m.attrInfo c in
  let max_stack, cb = split_n 2 ca.info in
  let max_local, cb = split_n 2 cb in
  let _ = fst (parse_two_bytes max_stack) in
  let max_local = fst (parse_two_bytes max_local) in
  let cb_len, cb = parse_four_bytes cb in
  let cb, _ = split_n cb_len cb in
  let (l : var array) = Array.make max_local (Obj.magic ()) in
  List.iteri
    (fun i arg ->
      match arg with
      | Int x -> if i < max_local then l.(i) <- Int x else ()
      | Float x -> if i < max_local then l.(i) <- Float x else ()
      | Str x -> if i < max_local then l.(i) <- Str x else ()
      | CRef x -> if i < max_local then l.(i) <- CRef x else ()
      | IARef x -> if i < max_local then l.(i) <- IARef x else ()
      | FARef x -> if i < max_local then l.(i) <- FARef x else ()
      | Void -> raise (UnexcpectedType "Unexcpected type in create frame"))
    args;
  let (Str mn) = resolve c.constPool m.fNameIndex in
  {
    class_file = c;
    method_name = mn;
    ip = 0;
    code = cb;
    locals = l;
    stack = Stack.empty_stack;
  }

let update_frame (f : jvmframe) (s : Stack.t) =
  { f with stack = s; ip = f.ip + 1 }

let update_frame_inc_ip (f : jvmframe) (s : Stack.t) (i : int) =
  { f with stack = s; ip = f.ip + i }

let update_frame_set_ip (f : jvmframe) (s : Stack.t) (i : int) =
  { f with stack = s; ip = i }

let get_name_and_args (cp : const list) (i : int) : int * string =
  let i = (List.nth cp (i - 1)).nameAndTypeIndex in
  let nti = List.nth cp (i - 1) in
  (nti.cNameIndex, (List.nth cp (nti.descIndex - 1)).cString)

(*find method based on fst index and snd is the descripter string to be parsed. *)

let is_concat_call (cp : const list) (i : int) =
  let bi = (List.nth cp (i - 1)).bootstrapMethodAttrIndex in
  let i = (List.nth cp (i - 1)).nameAndTypeIndex in
  let nti = List.nth cp (i - 1) in
  let (Str mn) = resolve cp nti.cNameIndex in
  if mn = "makeConcatWithConstants" then
    (true, bi, (List.nth cp (nti.descIndex - 1)).cString)
  else (false, 0, (List.nth cp (nti.descIndex - 1)).cString)

let ignore_method (cp : const list) (i : int) s =
  let i = (List.nth cp (i - 1)).classIndex in
  let ci = (List.nth cp (i - 1)).cNameIndex in
  let (Str mn) = resolve cp ci in
  if mn = "java/io/PrintStream" then (
    (let x =
       Stack.peek s |> function
       | Str o -> o
       | _ -> raise (UnexcpectedType "ignore_method got unexpected value")
     in
     print_endline x);
    true)
  else if mn = "java/lang/Object" then true
  else if mn = "java/lang/System" then true
  else false

let parse_descriptor str =
  let n = String.length str in
  let rec aux (i : int) (a : descriptor list) =
    if i = n then List.rev a
    else
      match str.[i] with
      | '(' | ')' -> aux (i + 1) a
      | 'I' -> aux (i + 1) (I :: a)
      | '[' -> (
          match str.[i + 1] with
          | 'I' -> aux (i + 2) (IA :: a)
          | 'F' -> aux (i + 2) (FA :: a))
      | 'F' -> aux (i + 1) (F :: a)
      | 'Z' -> aux (i + 1) (Z :: a)
      | 'L' ->
          let j = String.index_from str (i + 1) ';' in
          aux (j + 1) (S :: a)
      | 'V' -> aux (i + 1) (V :: a)
      | _ -> raise (UnexcpectedType "invalid descriptor ")
  in
  aux 0 []

let create_args (li : descriptor list) (stack : Stack.t) =
  let rec aux l s a =
    match l with
    | [ x ] ->
        (s, a) (*Leave last element as it is as last element is return type*)
    | x :: xs ->
        let a' = Stack.peek s :: a in
        aux xs (Stack.pop s) a'
    | [] -> failwith "Accidently passed return value in args"
  in
  aux li stack []

let make_string args =
  let rec aux s l =
    match l with [] -> List.rev s | x :: xs -> aux (Stack.to_string x :: s) xs
  in
  aux [] args

let make_concated_string str values =
  let parts = String.split_on_char '\001' str in
  let rec helper acc parts values =
    match (parts, values) with
    | [], [] -> acc
    | hd :: tl, [] -> acc ^ hd
    | hd :: tl, v :: vs -> helper (acc ^ hd ^ v) tl vs
    | _ -> raise (UnexcpectedType "Number of placeholders mismatch")
  in
  helper "" parts values

let rec find_bootstrapMethods_attribute (li : attr list) (c : cls) =
  let rec aux (l : attr list) =
    match l with
    | [] -> raise (NotFound "No BootStrap Methods attribute")
    | x :: xs ->
        let (Str mn) = resolve c.constPool x.aNameIndex in
        if mn = "BootstrapMethods" then x else aux xs
  in
  aux li

let get_format_string bi obj_cls =
  let bm = find_bootstrapMethods_attribute obj_cls.attributes obj_cls in
  let cpi = (List.nth bm.bootstrapMethods bi).bootstrapArguments in
  (*Assumed that there will be only one arg*)
  let (Str x) = resolve obj_cls.constPool (List.nth cpi 0) in
  x

let create_args_with_obj_ref (li : descriptor list) (stack : Stack.t) =
  let rec aux l s a =
    match l with
    | [ x ] ->
        (s, Stack.peek s :: a)
        (*Leave last element as it is as last element is return type*)
    | x :: xs ->
        let a' = Stack.peek s :: a in
        aux xs (Stack.pop s) a'
    | [] -> raise (UnexcpectedType "Accidently passed return value in args")
  in
  aux li stack []

let create_obj (clsname : string) : cls * var fl list ref =
  let obj_classfile = parse_other_class clsname in
  let fl =
    let rec aux l a c s =
      match l with
      | x :: xs -> (
          let (Str m) = resolve c x.descIndex in
          match m with
          | "I" ->
              let (Str mn) = resolve c x.fNameIndex in
              aux xs ({ name = mn; value = Int 0 } :: a) c s
          | "F" ->
              let (Str mn) = resolve c x.fNameIndex in
              aux xs ({ name = mn; value = Float 0. } :: a) c s
          | "Ljava/lang/String;" ->
              let (Str mn) = resolve c x.fNameIndex in
              aux xs ({ name = mn; value = Str "" } :: a) c s
          | _ ->
              raise
                (UnexcpectedType
                   ("Unknown DataType found while creating new obj " ^ m)))
      | [] ->
          if s <> "java/lang/Object" then
            let super_classfile = parse_other_class obj_classfile.super in
            aux super_classfile.fields a super_classfile.constPool
              super_classfile.super
          else a
    in
    aux obj_classfile.fields [] obj_classfile.constPool obj_classfile.super
  in
  (obj_classfile, ref fl)

let update_obj_ref name value obj_ref =
  obj_ref :=
    List.map
      (fun field -> if field.name = name then { field with value } else field)
      !obj_ref

let get_value_from_obj_ref name obj_ref =
  match List.find_opt (fun field -> field.name = name) !obj_ref with
  | Some field -> field.value
  | None -> raise (NotFound "No Field Found")

let debug = ref false

let rec exec (f : jvmframe) =
  (* let _ = read_line() in *)
  let op = List.nth f.code f.ip in
  let _ =
    if !debug then Stack.print_stack f.class_file.name f.method_name op f.stack
    else ()
  in
  match op with
  | 2 | 3 | 4 | 5 | 6 | 7 | 8 (*iconst*) ->
      exec @@ update_frame f @@ Stack.push (Int (op - 3)) f.stack
  | 11 | 12 | 13 (*fconst*) ->
      exec @@ update_frame f
      @@ Stack.push (Float (float_of_int op -. 11.)) f.stack
  | 16 (*bipush*) ->
      let v = List.nth f.code (f.ip + 1) in
      exec @@ update_frame_inc_ip f (Stack.push (Int v) f.stack) 2
  | 17 (*sipush*) ->
      let v = (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2) in
      let sv = signed_two_bytes v in
      exec @@ update_frame_inc_ip f (Stack.push (Int sv) f.stack) 3
  | 18 (*ldc*) ->
      let i = resolve f.class_file.constPool (List.nth f.code (f.ip + 1)) in
      exec @@ update_frame_inc_ip f (Stack.push i f.stack) 2
  | 21 (*iload*) ->
      let i = List.nth f.code (f.ip + 1) in
      let v = f.locals.(i) in
      exec @@ update_frame_inc_ip f (Stack.push v f.stack) 2
  | 25 (*aload*) ->
      let i = List.nth f.code (f.ip + 1) in
      let v = f.locals.(i) in
      exec @@ update_frame_inc_ip f (Stack.push v f.stack) 2
  | 26 | 27 | 28 | 29 (* iload *) -> (
      match f.locals.(op - 26) with
      | Int _ as x -> exec @@ update_frame f @@ Stack.push x f.stack
      | _ ->
          raise (UnexcpectedType "Required int found something else in iload"))
  | 34 | 35 | 36 | 37 (*fload_<n>*) ->
      exec @@ update_frame f @@ Stack.push f.locals.(op - 34) f.stack
  | 42 | 43 | 44 | 45 (*aload_<n>*) ->
      exec @@ update_frame f @@ Stack.push f.locals.(op - 42) f.stack
  | 46 (*iaload*) ->
      let (Int i) = Stack.peek f.stack in
      let new_stack = Stack.pop f.stack in
      let (IARef aref) = Stack.peek new_stack in
      let new_stack = Stack.pop new_stack in
      let v = Array.get !aref i in
      exec @@ update_frame f @@ Stack.push (Int v) new_stack
  | 48 (*faload*) ->
      let (Int i) = Stack.peek f.stack in
      let new_stack = Stack.pop f.stack in
      let (FARef aref) = Stack.peek new_stack in
      let new_stack = Stack.pop new_stack in
      let v = Array.get !aref i in
      exec @@ update_frame f @@ Stack.push (Float v) new_stack
  | 54 (*istore*) ->
      let i = List.nth f.code (f.ip + 1) in
      let x = Stack.peek f.stack in
      f.locals.(i) <- x;
      exec @@ update_frame_inc_ip f (Stack.pop f.stack) 2
  | 59 | 60 | 61 | 62 (*istore*) ->
      let x =
        Stack.peek f.stack |> function
        | Int o -> o
        | _ -> raise (UnexcpectedType "istore got unexpected value")
      in
      f.locals.(op - 59) <- Int x;
      exec @@ update_frame f @@ Stack.pop f.stack
  | 67 | 68 | 69 | 70 (*fstore*) ->
      let x =
        Stack.peek f.stack |> function
        | Float o -> o
        | _ -> raise (UnexcpectedType "fstore got unexpected value")
      in
      f.locals.(op - 67) <- Float x;
      exec @@ update_frame f @@ Stack.pop f.stack
  | 75 | 76 | 77 | 78 (*astore*) ->
      let x = Stack.peek f.stack in
      f.locals.(op - 75) <- x;
      exec @@ update_frame f (Stack.pop f.stack)
  | 79 (*iastore*) ->
      (*arrayref, index, value*)
      let (Int v) = Stack.peek f.stack in
      let new_stack = Stack.pop f.stack in
      let (Int i) = Stack.peek new_stack in
      let new_stack = Stack.pop new_stack in
      let (IARef arrayref) = Stack.peek new_stack in
      let new_stack = Stack.pop new_stack in
      let arr = !arrayref in
      arr.(i) <- v;
      (*TODO: handle NullPointerException ArrayIndexOutOfBoundsException exception*)
      exec @@ update_frame f new_stack
  | 81 (*fastore*) ->
      let (Float v) = Stack.peek f.stack in
      let new_stack = Stack.pop f.stack in
      let (Int i) = Stack.peek new_stack in
      let new_stack = Stack.pop new_stack in
      let (FARef arrayref) = Stack.peek new_stack in
      let new_stack = Stack.pop new_stack in
      let arr = !arrayref in
      arr.(i) <- v;
      (*TODO: handle NullPointerException ArrayIndexOutOfBoundsException exception*)
      exec @@ update_frame f new_stack
  | 87 (*pop*) -> exec @@ update_frame f @@ Stack.pop f.stack
  | 89 (*dup*) ->
      exec @@ update_frame f @@ Stack.push (Stack.peek f.stack) f.stack
  | 96 (*iadd*) -> exec @@ update_frame f @@ Stack.i_add f.stack
  | 98 (*fadd*) -> exec @@ update_frame f @@ Stack.f_add f.stack
  | 100 (*isub*) -> exec @@ update_frame f @@ Stack.i_sub f.stack
  | 102 (*fsub*) -> exec @@ update_frame f @@ Stack.f_sub f.stack
  | 104 (*imul*) -> exec @@ update_frame f @@ Stack.i_mul f.stack
  | 106 (*fmul*) -> exec @@ update_frame f @@ Stack.f_mul f.stack
  | 108 (*idiv*) -> exec @@ update_frame f @@ Stack.i_div f.stack
  | 110 (*fdiv*) -> exec @@ update_frame f @@ Stack.f_div f.stack
  | 132 (*iinc*) ->
      let i = List.nth f.code (f.ip + 1) in
      let c = signed_byte (List.nth f.code (f.ip + 2)) in
      let x =
        f.locals.(i) |> function
        | Int o -> o
        | _ -> raise (UnexcpectedType "iinc got unexpected value")
      in
      f.locals.(i) <- Int (x + c);
      exec @@ update_frame_inc_ip f f.stack 3
  | 134 (*i2f*) -> exec @@ update_frame f @@ Stack.i2f f.stack
  | 149 (*fcmpl*) -> exec @@ update_frame f @@ Stack.fcmpl f.stack
  | 150 (*fcmpg*) -> exec @@ update_frame f @@ Stack.fcmpg f.stack
  | 153 (*ifeq*) ->
      let b, s' = Stack.ifeq f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 154 (*ifne*) ->
      let b, s' = Stack.ifne f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 155 (*iflt*) ->
      let b, s' = Stack.iflt f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 156 (*ifge*) ->
      let b, s' = Stack.ifge f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 157 (*ifgt*) ->
      let b, s' = Stack.ifgt f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 158 (*ifle*) ->
      let b, s' = Stack.ifle f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 159 (*if_icmpeq*) ->
      let b, s' = Stack.if_icmpeq f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 160 (*if_icmpne*) ->
      let b, s' = Stack.if_icmpne f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 161 (*if_icmplt*) ->
      let b, s' = Stack.if_icmplt f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 162 (*if_icmpge*) ->
      let b, s' = Stack.if_icmpge f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 163 (*if_icmpgt*) ->
      let b, s' = Stack.if_icmpgt f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 164 (*if_icmple*) ->
      let b, s' = Stack.if_icmple f.stack in
      if b then
        let i =
          (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2)
        in
        exec @@ update_frame_set_ip f s' (f.ip + i)
      else exec @@ update_frame_inc_ip f s' 3
  | 167 (*goto*) ->
      let i = (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2) in
      let i = signed_two_bytes i in
      exec @@ update_frame_set_ip f f.stack (f.ip + i)
  | 172 (*ireturn*) ->
      Stack.peek f.stack (*Caller should push this value to the stack*)
  | 174 (*freturn*) -> Stack.peek f.stack
  | 176 (*areturn*) -> Stack.peek f.stack
  | 177 -> Void
  | 178 (*getstatic*) ->
      let i = (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2) in
      if ignore_method f.class_file.constPool i f.stack then
        exec @@ update_frame_inc_ip f f.stack 3
      else raise (NotFound "get static instruction not impl")
  | 180 (*getfield*) ->
      let i = (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2) in
      let fi, d = get_name_and_args f.class_file.constPool i in
      let (Str fn) = resolve f.class_file.constPool fi in
      let obj =
        Stack.peek f.stack |> function
        | CRef o -> o
        | _ -> raise (UnexcpectedType "getfield got unexpected value")
      in
      let new_stack = Stack.pop f.stack in
      let obj = snd obj in
      let v = get_value_from_obj_ref fn obj in
      exec @@ update_frame_inc_ip f (Stack.push v new_stack) 3
  | 181 (*putfield*) ->
      let i = (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2) in
      let fi, d = get_name_and_args f.class_file.constPool i in
      let (Str fn) = resolve f.class_file.constPool fi in
      let v = Stack.peek f.stack in
      let new_stack = Stack.pop f.stack in
      let obj =
        Stack.peek new_stack |> function
        | CRef o -> o
        | _ -> raise (UnexcpectedType "putfield got unexpected value")
      in
      let new_stack = Stack.pop new_stack in
      let obj = snd obj in
      update_obj_ref fn v obj;
      exec @@ update_frame_inc_ip f new_stack 3
  | 182 (*invokevirtual*) -> (
      let i = (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2) in
      if ignore_method f.class_file.constPool i f.stack then
        exec @@ update_frame_inc_ip f (Stack.pop f.stack) 3
      else
        let mi, d = get_name_and_args f.class_file.constPool i in
        let new_stack, args =
          create_args_with_obj_ref (parse_descriptor d) f.stack
        in
        let (Str obj_cls_name) =
          resolve f.class_file.constPool
            (List.nth f.class_file.constPool (i - 1)).classIndex
        in
        let obj_cls = parse_other_class obj_cls_name in
        let (Str mn) = resolve f.class_file.constPool mi in
        let obj_cls, m = find_method_by_name obj_cls mn d in
        let new_frame = create_frame m obj_cls args in
        let return_val = exec new_frame in
        let new_stack = Stack.pop new_stack in
        match return_val with
        | Void -> exec @@ update_frame_inc_ip f new_stack 3
        | _ -> exec @@ update_frame_inc_ip f (Stack.push return_val new_stack) 3
      )
  | 183 (*invokespecial*) -> (
      let i = (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2) in
      if ignore_method f.class_file.constPool i f.stack then
        exec @@ update_frame_inc_ip f (Stack.pop f.stack) 3
      else
        let mi, d = get_name_and_args f.class_file.constPool i in
        let new_stack, args =
          create_args_with_obj_ref (parse_descriptor d) f.stack
        in
        let (Str obj_cls_name) =
          resolve f.class_file.constPool
            (List.nth f.class_file.constPool (i - 1)).classIndex
        in
        let obj_cls = parse_other_class obj_cls_name in
        let (Str mn) = resolve f.class_file.constPool mi in
        let obj_cls, m = find_method_by_name obj_cls mn d in
        let new_frame = create_frame m obj_cls args in
        let return_val = exec new_frame in
        let new_stack = Stack.pop new_stack in
        match return_val with
        | Void -> exec @@ update_frame_inc_ip f new_stack 3
        | _ -> exec @@ update_frame_inc_ip f (Stack.push return_val new_stack) 3
      )
  | 184 (*invokestatic*) -> (
      let i = (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2) in
      let mi, d = get_name_and_args f.class_file.constPool i in
      let new_stack, args = create_args (parse_descriptor d) f.stack in
      let m = find_method f.class_file.methods mi in
      let new_frame = create_frame m f.class_file args in
      let return_val = exec new_frame in
      match return_val with
      | Void -> exec @@ update_frame_inc_ip f new_stack 3
      | _ -> exec @@ update_frame_inc_ip f (Stack.push return_val new_stack) 3)
  | 186 (*invokedynamic*) ->
      let i = (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2) in
      let ignore_call, bi, d = is_concat_call f.class_file.constPool i in
      if ignore_call then
        let new_stack, args = create_args (parse_descriptor d) f.stack in
        let str_list = make_string args in
        let format_string = get_format_string bi f.class_file in
        let final_str = make_concated_string format_string str_list in
        exec @@ update_frame_inc_ip f (Stack.push (Str final_str) new_stack) 5
      else raise (NotFound "No impl for invokedynamic")
  | 187 (*new*) ->
      let i = (List.nth f.code (f.ip + 1) * 256) + List.nth f.code (f.ip + 2) in
      let (Str cn) = resolve f.class_file.constPool i in
      let obj = CRef (create_obj cn) in
      exec @@ update_frame_inc_ip f (Stack.push obj f.stack) 3
  | 188 (*newarray*) -> (
      let t = List.nth f.code (f.ip + 1) in
      let (Int n) = Stack.peek f.stack in
      let new_stack = Stack.pop f.stack in
      match t with
      | 4 (*boolean*) -> failwith "this type of array is not supported"
      | 5 (*char*) -> failwith "this type of array is not supported"
      | 6 (*float*) ->
          let aref = ref (Array.make n 0.) in
          let new_stack = Stack.push (FARef aref) new_stack in
          exec @@ update_frame_inc_ip f new_stack 2
      | 7 (*double*) -> failwith "this type of array is not supported"
      | 8 (*byte*) ->
          let aref = ref (Array.make n 0) in
          let new_stack = Stack.push (IARef aref) new_stack in
          exec @@ update_frame_inc_ip f new_stack 2
      | 9 (*short*) ->
          let aref = ref (Array.make n 0) in
          let new_stack = Stack.push (IARef aref) new_stack in
          exec @@ update_frame_inc_ip f new_stack 2
      | 10 (*int*) ->
          let aref = ref (Array.make n 0) in
          let new_stack = Stack.push (IARef aref) new_stack in
          exec @@ update_frame_inc_ip f new_stack 2
      | 11 (*long*) -> failwith "this type of array is not supported")
  | 190 (*arraylength*) -> (
      let aref = Stack.peek f.stack in
      let new_stack = Stack.pop f.stack in
      match aref with
      | IARef a ->
          exec @@ update_frame f (Stack.push (Int (Array.length !a)) new_stack)
      | FARef a ->
          exec @@ update_frame f (Stack.push (Int (Array.length !a)) new_stack)
      | _ -> failwith "Array length of this type of array not supported")
  | _ ->
      print_endline (string_of_int op);
      raise (NotFound "OOPS")

let find_main c =
  let rec aux i =
    let index = (List.nth c.methods i).fNameIndex in
    let (Str mn) = resolve c.constPool index in
    match mn with "main" -> index | _ -> aux (i + 1)
  in
  aux 0
;;

let class_file = parse_file (Sys.argv.(1) ^ ".class") in
let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with "-debug" -> debug := true | _ -> ()
  done
in
let main_index = find_main class_file in
let main = find_method class_file.methods main_index in
let main_frame = create_frame main class_file [] in
(*assumed that main will have no args*)
exec main_frame
