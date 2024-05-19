type 'a t = { parent : ('a, 'a) Hashtbl.t; size : ('a, int) Hashtbl.t }

let create () = { parent = Hashtbl.create 10; size = Hashtbl.create 10 }

let make_set (uf : 'a t) x : unit =
  Hashtbl.add uf.parent x x;
  Hashtbl.add uf.size x 1

let rec find (uf : 'a t) (x : 'a) : 'a =
  try
    let p = Hashtbl.find uf.parent x in
    if p <> x then (
      let root = find uf p in
      Hashtbl.replace uf.parent x root;
      root)
    else x
  with Not_found ->
    make_set uf x;
    x

let union (uf : 'a t) (x : 'a) (y : 'a) : unit =
  let root_x = find uf x in
  let root_y = find uf y in
  if root_x <> root_y then
    let size_x = Hashtbl.find uf.size root_x in
    let size_y = Hashtbl.find uf.size root_y in
    if size_x < size_y then (
      Hashtbl.replace uf.parent root_x root_y;
      Hashtbl.replace uf.size root_y (size_x + size_y))
    else (
      Hashtbl.replace uf.parent root_y root_x;
      Hashtbl.replace uf.size root_x (size_x + size_y))
