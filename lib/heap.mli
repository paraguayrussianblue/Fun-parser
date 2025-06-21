(* CSE322 Compiler Assignment 2 *)
(* Heap *)

type loc
type 'a heap

val fresh_loc : 'a heap -> loc
val get_loc : 'a heap -> loc -> 'a
val set_loc : 'a heap -> loc -> 'a -> unit
val loc_to_string : loc -> string

val empty : unit -> 'a heap
val print_heap : 'a heap -> ('a -> unit) -> unit
