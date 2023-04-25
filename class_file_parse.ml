open Types


(*Helper functions to parse bytes*)
let signed_byte x = if x > 127 then -((lnot x land 0xFF) + 1) else x

let signed_two_bytes x =
  if x land 0x8000 > 0 then -((lnot x land 0xFFFF) + 1) else x

let parse_eight_bytes l =
  let rec aux acc n = function
    | [] -> if n = 8 then (acc, []) else failwith "parse_eight_bytes failed"
    | x :: xs ->
        if n = 8 then (acc, x :: xs)
        else
          let shifted_byte = Int64.shift_left acc 8 in
          aux (Int64.add shifted_byte (Int64.of_int x)) (n + 1) xs
  in
  aux 0L 0 l

let bytes_to_string n l =
  let rec aux byts i = function
    | [] -> (byts, [])
    | b :: l ->
        if i = n then (byts, b :: l)
        else
          let _ = Bytes.set byts i (Char.chr b) in
          aux byts (i + 1) l
  in
  let byts, l = aux (Bytes.create n) 0 l in
  (Bytes.to_string byts, l)

let parse_four_bytes l =
  match l with
  | a :: b :: c :: d :: xs ->
      let i =
        Int.shift_left a 24 lor Int.shift_left b 16 lor Int.shift_left c 8 lor d
      in
      (i, xs)
  | _ -> raise (UnexcpectedType "Insufficent number of bytes found")

let parse_two_bytes l =
  match l with
  | a :: b :: xs ->
      let i = Int.shift_left a 8 lor b in
      (i, xs)
  | _ -> raise (UnexcpectedType "Insufficent number of bytes found")

let parse_magicword l =
  match l with
  | 202 :: 254 :: 186 :: 190 :: xs -> xs
  | _ -> raise (UnexcpectedType "Magic word is incorrect")

let parse_version l =
  match l with
  | a :: b :: c :: d :: xs -> xs
  | _ -> raise (UnexcpectedType "Insufficent number of bytes found")

let parse_constant_pool_count l =
  match l with
  | a :: b :: xs -> ((a * 256) + b, xs)
  | _ -> raise (UnexcpectedType "Insufficent number of bytes found")

let get_bytes fn =
  let inc = open_in_bin fn in
  let rec go sofar =
    match input_char inc with
    | b -> go (Char.code b :: sofar)
    | exception End_of_file -> List.rev sofar
  in
  let res = go [] in
  close_in inc;
  res

let parse_numeric_float i = 
    let s = if Int.shift_right i 31 = 0 then 1 else (-1) in
    let e = (Int.shift_right i 23) land 0xff in
    let m = if e = 0 then Int.shift_left (i land 0x7fffff) 1
            else (i land 0x7fffff) lor 0x800000 
    in (float_of_int s) *. (float_of_int m) *. (Float.pow 2. (float_of_int (e-150))) ;;
    
let parse_float_number l = 
  let (bits,l') = parse_four_bytes l in
  let b = (match bits with 
  | 0x7f800000 -> Float.infinity 
  | 0xff800000 -> Float.neg_infinity
  | x -> if (x >= 0x7f800001 && x <= 0x7fffffff) || (x >= 0xff800001 && x <= 0xffffffff) then Float.nan
         else parse_numeric_float x )
        in (b,l');;

let base_const =
  {
    tag = -99999;
    cNameIndex = -99999;
    classIndex = -99999;
    nameAndTypeIndex = -99999;
    stringIndex = -99999;
    descIndex = -99999;
    bootstrapMethodAttrIndex = -99999;
    referenceKind = -99999;
    referenceIndex = -99999;
    cString = "NaN";
    float_num = -99999.;
  }

(* functions to parse class file structure*)
let parse_constant_pool l count =
  let rec aux l c const_pool =
    if c >= count then (const_pool, l)
    else
      let constant, newlist =
        match l with
        | 7 :: xs (* CONSTANT_Class  *) ->
            let name_index, l' = parse_two_bytes xs in
            ({ base_const with tag = 7; cNameIndex = name_index }, l')
        | 10 :: xs (* CONSTANT_Methodref  *) ->
            let class_index, l' = parse_two_bytes xs in
            let name_and_type_index, l'' = parse_two_bytes l' in
            ( {
                base_const with
                tag = 10;
                classIndex = class_index;
                nameAndTypeIndex = name_and_type_index;
              },
              l'' )
        | 9 :: xs (*CONSTANT_Fieldref*) ->
            let class_index, l' = parse_two_bytes xs in
            let name_and_type_index, l'' = parse_two_bytes l' in
            ( {
                base_const with
                tag = 9;
                classIndex = class_index;
                nameAndTypeIndex = name_and_type_index;
              },
              l'' )
        | 11 :: xs (* CONSTANT_InterfaceMethodref  *) ->
            raise (NotFound "notimpl CONSTANT_InterfaceMethodref")
        | 8 :: xs (* CONSTANT_String  *) ->
            let si, l' = parse_two_bytes xs in
            ({ base_const with tag = 8; stringIndex = si }, l')
        | 3 :: xs (* CONSTANT_Integer  *) ->
            raise (NotFound "notimpl CONSTANT_Integer")
        | 4 :: xs (* CONSTANT_Float  *) ->
            let n,l' = parse_float_number xs in
            ({base_const with tag=4; float_num=n},l')
        | 5 :: xs (* CONSTANT_Long  *) ->
            raise (NotFound "notimpl CONSTANT_Long")
        | 6 :: xs (* CONSTANT_Double  *) ->
            raise (NotFound "notimpl CONSTANT_Double")
        | 12 :: xs (* CONSTANT_NameAndType  *) ->
            let name_index, l' = parse_two_bytes xs in
            let descriptor_index, l'' = parse_two_bytes l' in
            ( {
                base_const with
                tag = 12;
                cNameIndex = name_index;
                descIndex = descriptor_index;
              },
              l'' )
        | 1 :: xs (* CONSTANT_Utf8  *) ->
            let length, l' = parse_two_bytes xs in
            let string_val, l'' = bytes_to_string length l' in
            ({ base_const with tag = 1; cString = string_val }, l'')
        | 15 :: x :: xs (* CONSTANT_MethodHandle  *) ->
            let rk = x in
            let ri, l'' = parse_two_bytes xs in
            ( {
                base_const with
                tag = 15;
                referenceIndex = ri;
                referenceKind = rk;
              },
              l'' )
        | 16 :: xs (* CONSTANT_MethodType  *) ->
            raise (NotFound "notimpl CONSTANT_MethodType")
        | 17 :: xs (* CONSTANT_Dynamic  *) ->
            raise (NotFound "notimpl CONSTANT_Dynamic")
        | 18 :: xs (* CONSTANT_InvokeDynamic  *) ->
            let bi, l' = parse_two_bytes xs in
            let name_and_type_index, l'' = parse_two_bytes l' in
            ( {
                base_const with
                tag = 18;
                nameAndTypeIndex = name_and_type_index;
                bootstrapMethodAttrIndex = bi;
              },
              l'' )
        | 19 :: xs (* CONSTANT_Module  *) ->
            raise (NotFound "notimpl CONSTANT_Module")
        | 20 :: xs (* CONSTANT_Package  *) ->
            raise (NotFound "notimpl CONSTANT_Package")
        | _ -> raise (NotFound "unknown tag")
      in
      aux newlist (c + 1) (const_pool @ [ constant ])
  in
  aux l 0 []

let rec resolve cp index =
  let const = List.nth cp (index - 1) in
  if const.tag = 1 then Str(const.cString)
  else if const.tag = 4 then Float(const.float_num)
  else if const.tag = 7 then resolve cp const.cNameIndex
  else if const.tag = 8 then resolve cp const.stringIndex
  else Str("")

let split_n n lst =
  let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | hd :: tl ->
        if i < n then aux (hd :: acc) (i + 1) tl else (List.rev acc, hd :: tl)
  in
  aux [] 0 lst

let parse_attribute_info l count =
  let rec aux b c attr_info =
    if c > count then (attr_info, b)
    else
      let attribute_name_index, b = parse_two_bytes b in
      (* let _ = print_endline "parse_attribute_info CASE" in *)
      let attr_len, b = parse_four_bytes b in
      let attr_bytes, b = split_n attr_len b in
      let info =
        [
          {
            aNameIndex = attribute_name_index;
            len = attr_len;
            info = attr_bytes;
            bootstrapMethods = [];
          };
        ]
      in
      aux b (c + 1) (attr_info @ info)
  in
  aux l 1 []

let parse_bootstrap_attr l count =
  let rec aux i b args =
    if i > count then (args, b)
    else
      let arg, b = parse_two_bytes b in
      aux (i + 1) b (arg :: args)
  in
  aux 1 l []

let parse_bootstrap_methods l count =
  let rec aux i b methods =
    if i > count then (List.rev methods, b)
    else
      let bootstrap_method_ref, b = parse_two_bytes b in
      let num_bootstrap_arguments, b = parse_two_bytes b in
      let bootstrap_arguments, b =
        parse_bootstrap_attr b num_bootstrap_arguments
      in
      aux (i + 1) b
        ({
           bootstrapMethodRef = bootstrap_method_ref;
           numBootstrapArguments = num_bootstrap_arguments;
           bootstrapArguments = bootstrap_arguments;
         }
        :: methods)
  in
  aux 1 l []

let parse_attribute_info_with_bootstrap l count cp =
  let rec aux b c attr_info =
    if c > count then (attr_info, b)
    else
      let attribute_name_index, b = parse_two_bytes b in
      let Str(mn) = resolve cp attribute_name_index in
      if mn = "BootstrapMethods" then
        let attr_len, b = parse_four_bytes b in
        let attr_method_num, b = parse_two_bytes b in
        let bootstrap_methods, b = parse_bootstrap_methods b attr_method_num in
        let info =
          [
            {
              aNameIndex = attribute_name_index;
              len = attr_len;
              info = [];
              bootstrapMethods = bootstrap_methods;
            };
          ]
        in
        aux b (c + 1) (attr_info @ info)
      else
        (* let _ = print_endline "ELSE CASE" in *)
        let attr_len, b = parse_four_bytes b in
        let attr_bytes, b = split_n attr_len b in
        let info =
          [
            {
              aNameIndex = attribute_name_index;
              len = attr_len;
              info = attr_bytes;
              bootstrapMethods = [];
            };
          ]
        in
        aux b (c + 1) (attr_info @ info)
  in
  aux l 1 []

let parse_method_info l count : field list * int list =
  let rec aux b c method_info =
    if c > count then (method_info, b)
    else
      let access_flags, b = parse_two_bytes b in
      let name_index, b = parse_two_bytes b in
      let descriptor_index, b = parse_two_bytes b in
      let attributes_count, b = parse_two_bytes b in
      let attribute_info, b = parse_attribute_info b attributes_count in
      let info =
        [
          {
            accessFlag = access_flags;
            fNameIndex = name_index;
            descIndex = descriptor_index;
            attrCount = attributes_count;
            attrInfo = attribute_info;
          };
        ]
      in
      aux b (c + 1) (method_info @ info)
  in
  aux l 1 []

let parse_field_info l count : field list * int list =
  let rec aux b c field_info =
    if c > count then (field_info, b)
    else
      let access_flags, b = parse_two_bytes b in
      let name_index, b = parse_two_bytes b in
      let descriptor_index, b = parse_two_bytes b in
      let attributes_count, b = parse_two_bytes b in
      let attribute_info, b = parse_attribute_info b attributes_count in
      let info =
        [
          {
            accessFlag = access_flags;
            fNameIndex = name_index;
            descIndex = descriptor_index;
            attrCount = attributes_count;
            attrInfo = attribute_info;
          };
        ]
      in
      aux b (c + 1) (field_info @ info)
  in
  aux l 1 []

let parse_interfaces l count : interface list * int list =
  let rec aux b c interface_info =
    if c > count then (interface_info, b)
    else
      let name_index, b = parse_two_bytes b in
      let info = [ { iNameIndex = name_index } ] in
      aux b (c + 1) (interface_info @ info)
  in
  aux l 1 []

let parse_file file =
  let b = get_bytes file in
  let b = parse_magicword b in
  let b = parse_version b in
  let constant_pool_count, b = parse_constant_pool_count b in
  let constant_pool, b =
    if constant_pool_count > 0 then
      parse_constant_pool b (constant_pool_count - 1)
    else ([], b)
  in
  let access_flags, b = parse_two_bytes b in
  let this_class, b = parse_two_bytes b in
  let super_class, b = parse_two_bytes b in
  let interfaces_count, b = parse_two_bytes b in
  let interfaces, b =
    if interfaces_count > 0 then parse_interfaces b interfaces_count else ([], b)
  in
  let fields_count, b = parse_two_bytes b in
  let field_info, b =
    if fields_count > 0 then parse_field_info b fields_count else ([], b)
  in
  let method_count, b = parse_two_bytes b in
  let method_info, b = parse_method_info b method_count in
  let attributes_count, b = parse_two_bytes b in
  let attribute_info, b =
    parse_attribute_info_with_bootstrap b attributes_count constant_pool in
  let name =  (let Str(x) = resolve constant_pool this_class in x )  in
  let super_name =  (let Str(x) = resolve constant_pool super_class in x )  
  in
  {
    constPool = constant_pool;
    name = name;
    super = super_name;
    accessFlags = access_flags;
    interfaces;
    fields = field_info;
    methods = method_info;
    attributes = attribute_info;
  }
