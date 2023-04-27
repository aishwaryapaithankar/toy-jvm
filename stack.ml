(* #use "types.ml";; *)
open Types

module Stack = struct
  type t = var list

  let empty_stack : t = []
  let push (x : var) (s : t) = x :: s
  let peek (s : t) : var = match s with x :: _ -> x | [] -> raise EmptyStack
  let pop (s : t) : t = match s with x :: s -> s | [] -> raise EmptyStack

  let i2f (l : t) : t =
    match l with
    | Int x :: s -> Float (float_of_int x) :: s
    | _ -> raise (UnexcpectedType "Int required found something else in arith")

  let iarith f (l : t) : t =
    match l with
    | Int x :: Int y :: s -> Int (f y x) :: s
    | _ -> raise (UnexcpectedType "Int required found something else in iarith")

  let farith f (l : t) : t =
    match l with
    | Float x :: Float y :: s -> Float (f y x) :: s
    | _ ->
        raise (UnexcpectedType "Float required found something else in farith")

  let i_add = iarith ( + )
  let i_sub = iarith ( - )
  let i_mul = iarith ( * )
  let i_div = iarith ( / )
  let f_mul = farith ( *. )
  let f_div = farith ( /. )
  let f_sub = farith ( -. )
  let f_add = farith ( +. )

  let if_icmpeq (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y = x, s)
    | Float x :: Float y :: s -> (y = x, s)
    | _ ->
        raise
          (UnexcpectedType
             "Int/Float required found something else in if_icmpeq")

  let if_icmpne (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y <> x, s)
    | Float x :: Float y :: s -> (y <> x, s)
    | _ ->
        raise
          (UnexcpectedType
             "Int/Float required found something else in if_icmpne")

  let if_icmplt (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y < x, s)
    | Float x :: Float y :: s -> (y < x, s)
    | _ ->
        raise
          (UnexcpectedType
             "Int/Float required found something else in if_icmplt")

  let if_icmpge (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y >= x, s)
    | Float x :: Float y :: s -> (y >= x, s)
    | _ ->
        raise
          (UnexcpectedType
             "Int/Float required found something else in if_icmpge")

  let if_icmpgt (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y > x, s)
    | Float x :: Float y :: s -> (y > x, s)
    | _ ->
        raise
          (UnexcpectedType
             "Int/Float required found something else in if_icmpgt")

  let if_icmple (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y <= x, s)
    | Float x :: Float y :: s -> (y <= x, s)
    | _ ->
        raise
          (UnexcpectedType
             "Int/Float required found something else in if_icmple")

  let fcmpg (l : t) : t =
    match l with
    | Float x :: Float y :: s ->
        if y = Float.nan || x = Float.nan then Int 1 :: s
        else if y > x then Int 1 :: s
        else if y = x then Int 0 :: s
        else if y < x then Int (-1) :: s
        else s
    | _ ->
        raise
          (UnexcpectedType "Int/Float required found something else in fcmpg")

  let fcmpl (l : t) : t =
    match l with
    | Float x :: Float y :: s ->
        if y = Float.nan || x = Float.nan then Int (-1) :: s
        else if y > x then Int 1 :: s
        else if y = x then Int 0 :: s
        else if y < x then Int (-1) :: s
        else s
    | _ ->
        raise
          (UnexcpectedType "Int/Float required found something else in fcmpg")

  let compare_int (op : int -> int -> bool) (l : t) : bool * t =
    match l with
    | Int x :: s -> (op x 0, s)
    | _ ->
        raise
          (UnexcpectedType "Int required found something else in compare_int")

  let ifle = compare_int ( <= )
  let ifeq = compare_int ( = )
  let ifne = compare_int ( <> )
  let iflt = compare_int ( < )
  let ifge = compare_int ( >= )
  let ifgt = compare_int ( > )

  let to_string = function
    | Int n -> string_of_int n
    | Float n -> string_of_float n
    | Str s -> s
    | CRef x -> "Obj Ref of " ^ (fst x).name
    | IARef x ->
        "["
        ^ (Array.to_list !x |> List.map string_of_int |> String.concat ", ")
        ^ "]"
    | FARef x ->
        "["
        ^ (Array.to_list !x |> List.map string_of_float |> String.concat ", ")
        ^ "]"
    | Void -> " "

  let print_stack c n op s =
    let str_s = List.map to_string s in
    let str =
      c ^ " class : " ^ n ^ " method : " ^ string_of_int op ^ " instruction: ["
      ^ String.concat "," str_s ^ "]"
    in
    print_endline str
end

type jvmframe = {
  class_file : cls;
  method_name : string;
  ip : int;
  code : int list;
  locals : var array;
  stack : Stack.t;
}
