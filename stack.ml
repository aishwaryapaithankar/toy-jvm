(* #use "types.ml";; *)
open Types

module Stack = struct
  type t = var list

  let empty_stack : t = []

  let push (x:var) (s:t) = x :: s
  
  let peek (s:t) : var =
    match s with
    | x :: _ -> x
  
  let pop (s:t) : t =
    match s with
    | x :: s -> s;;
  
  let i_add ((Int(x)::Int(y)::s) : t) : t = Int(y+x)::s

  let i_sub ((Int(x)::Int(y)::s) : t) : t = Int(y-x)::s

  let i_mul ((Int(x)::Int(y)::s) : t) : t = Int(y*x)::s

  let i_div ((Int(x)::Int(y)::s) : t) : t = Int(y/x)::s

  let if_icmpeq ((Int(x)::Int(y)::s) : t) : (bool*t) = (y=x),s 
  
  let if_icmpne ((Int(x)::Int(y)::s) : t) : (bool*t) = (y<>x),s 

  let if_icmplt ((Int(x)::Int(y)::s) : t) : (bool*t) = (y<x),s 

  let if_icmpge ((Int(x)::Int(y)::s) : t) : (bool*t) = (y>=x),s 
  
  let if_icmpgt ((Int(x)::Int(y)::s) : t) : (bool*t) = (y>x),s 

  let if_icmple ((Int(x)::Int(y)::s) : t) : (bool*t) = (y<=x),s 

  let ifle ((Int(x)::s) : t) : (bool*t) = (x <= 0),s

  let ifeq ((Int(x)::s) : t) : (bool*t) = (x = 0),s

  let to_string = function
  | Int n -> string_of_int n
  | Str s -> s
  | CRef x -> "Obj Ref of " ^ (fst x).name
  | Void -> " "

  let print_stack c n op s = 
    let str_s = List.map to_string s in
    let str = c ^ " class : " ^ n ^" method : "^(string_of_int op)^" instruction: [" ^ (String.concat "," str_s) ^ "]" in
    print_endline str
end;;

type jvmframe = {
  class_file: cls;
  method_name: string;
	ip: int;
	code: int list;
	locals: var array;
	stack: Stack.t;
}
(* let s = (Stack.empty_stack |> Stack.push (Int(3)) |> Stack.push (Str("a"))) in
let t = Stack.pop s in
Stack.peek t;; *)
(* 
 Stack.empty_stack |> Stack.peek;; *)