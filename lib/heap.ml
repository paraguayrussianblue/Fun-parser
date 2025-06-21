(* CSE322 Compiler Assignment 2 *)
(* Heap *)

type loc = int
type 'a heap = loc ref * (loc, 'a) Hashtbl.t

let fresh_loc (next_loc, _) =
  let l = !next_loc in
  next_loc := l + 1; l

(* raises Mem.Not_found if loc is not in heap *)
let get_loc (_,m) loc = Hashtbl.find m loc
let set_loc (_,m) loc value = Hashtbl.add m loc value

let loc_to_string loc = Int.to_string loc

let empty () = (ref 0, Hashtbl.create 512)

let print_heap ((next_loc, mem) as heap) print_alpha =
  let print_loc (loc, v) =
    (print_string "L"; print_string (Int.to_string loc);
     print_string ": "; print_alpha v; print_string "  ") in
  let limit = !next_loc in
  let rec loop (i:int) = (if i < limit
    then (print_loc(i, get_loc heap i); loop (i+1))
    else ()) in
  if limit = 0 then print_string "empty" else loop (0)
