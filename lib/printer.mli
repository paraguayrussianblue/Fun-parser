(* CSE322 Compiler Assignment 2 *)
(*
  Auxiliary Module for Printing a Fun Program
  Original Authors: Yitzhak Mandelbaum, etc.
*)

exception PrintError

val print_prog : Absyn.prog -> unit
val print_prog_as_is : Absyn.prog -> unit
val print_fun : Absyn.func -> unit
val print_exp : Absyn.exp -> unit
val print_value : Eval.value -> unit
