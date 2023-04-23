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

  let i_add (l : t) : t =
    match l with
    | Int x :: Int y :: s -> Int (y + x) :: s
    | _ -> raise (UnexcpectedType "Int required found something else in iadd")

  let i_sub (l : t) : t =
    match l with
    | Int x :: Int y :: s -> Int (y - x) :: s
    | _ -> raise (UnexcpectedType "Int required found something else in isub")

  let i_mul (l : t) : t =
    match l with
    | Int x :: Int y :: s -> Int (y * x) :: s
    | _ -> raise (UnexcpectedType "Int required found something else in imul")

  let i_div (l : t) : t =
    match l with
    | Int x :: Int y :: s -> Int (y / x) :: s
    | _ -> raise (UnexcpectedType "Int required found something else in idiv")

  let if_icmpeq (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y = x, s)
    | _ -> raise (UnexcpectedType "Int required found something else in if_icmpeq")

  let if_icmpne (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y <> x, s)
    | _ -> raise (UnexcpectedType "Int required found something else in if_icmpne")

  let if_icmplt (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y < x, s)
    | _ -> raise (UnexcpectedType "Int required found something else in if_icmplt")

  let if_icmpge (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y >= x, s)
    | _ -> raise (UnexcpectedType "Int required found something else in if_icmpge")

  let if_icmpgt (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y > x, s)
    | _ -> raise (UnexcpectedType "Int required found something else in if_icmpgt")

  let if_icmple (l : t) : bool * t =
    match l with
    | Int x :: Int y :: s -> (y <= x, s)
    | _ -> raise (UnexcpectedType "Int required found something else in if_icmple")

  let ifle (l : t) : bool * t =
    match l with
    | Int x :: s -> (x <= 0, s)
    | _ -> raise (UnexcpectedType "Int required found something else in ifle")

  let ifeq (l : t) : bool * t =
    match l with
    | Int x :: s -> (x = 0, s)
    | _ -> raise (UnexcpectedType "Int required found something else in ifeq")

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