
let empty_stack = ()

let push x stack = x,stack

let peek (stack: 'a*'b) =
  match stack with
  | (x, _) -> x

let pop stack =
  match stack with
  | (x, s) -> s;;

let add ((x, (y, s)):(int * (int * 'a))) : (int * 'a) = (x + y), s;;

