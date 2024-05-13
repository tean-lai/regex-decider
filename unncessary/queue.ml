(* Ripped from https://cs3110.github.io/textbook/chapters/modules/functional_data_structures.html?highlight=batched#queues *)
type 'a t = { o : 'a list; i : 'a list }

exception Empty

let empty = { o = []; i = [] }
let is_empty = function { o = []; i } -> true | _ -> false

let enqueue x = function
  | { o = []; i } -> { o = [ x ]; i = [] }
  | { o; i } -> { o; i = x :: i }

let front = function { o = []; i } -> raise Empty | { o = h :: _; i } -> h

let dequeue = function
  | { o = []; i } -> raise Empty
  | { o = [ _ ]; i } -> { o = List.rev i; i = [] }
  | { o = _ :: t; i } -> { o = t; i }

let size { o; i } = List.(length o + length i)
let to_list { o; i } = o @ List.rev i
