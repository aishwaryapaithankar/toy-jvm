(* #use "types.ml";; *)
open Types

module Stack = struct
  type t = var list

  let empty_stack : t = []
  let push (x : var) (s : t) = x :: s

  let peek (s : t) : var =
    match s with x :: _ -> x | [] -> raise EmptyStack

  let pop (s : t) : t =
    match s with x :: s -> s | [] -> raise EmptyStack

  let arith f (l : t) : t =
    match l with
    | Int x :: Int y :: s -> Int (f y x) :: s
    | _ -> raise (UnexcpectedType "Int required found something else in arith")
  
  let i_add = arith (+)
  let i_sub = arith (-)

  let i_mul = arith ( * )

  let i_div = arith (/)

  let if_cmp f (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (f x y, s)
    | _ -> raise (UnexcpectedType "Int required found something else in if_cmp") 

  let if_icmpeq = if_cmp (=)

  let if_icmpne = if_cmp (<>)

  let if_icmplt = if_cmp (<)

  let if_icmpge = if_cmp (>=)

  let if_icmpgt = if_cmp (>)
  
  let if_icmple = if_cmp (<=)

  let compare_int (op : int -> int -> bool) (l : t) : bool * t =
    match l with
    | Int x :: s -> (op x 0, s)
    | _ -> raise (UnexcpectedType "Int required found something else in compare_int")

  let ifle = compare_int (<=)
  
  let ifeq = compare_int (=)
 
  let ifne = compare_int (<>)
  
  let iflt = compare_int (<)

  let ifge = compare_int (>=)
  
  let ifgt = compare_int (>)
  
  let to_string = function
    | Int n -> string_of_int n
    | Str s -> s
    | CRef x -> "Obj Ref of " ^ (fst x).name
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