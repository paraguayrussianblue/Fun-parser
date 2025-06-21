(* CSE322 Compiler Assignment 2 *)
(*
  Simple Fun Interpreter
  Original Authors: David Walker, Yitzhak Mandelbaum, Andrew Appel, etc.
*)

type value =
  | IntV of int            (* integer value *)
  | FunV of Absyn.func     (* function value *)
  | ExternV of Absyn.id    (* external function value *)
  | TupleV of value list   (* tuple value *)
  | LocV of Heap.loc       (* new - location value*)

(*
  Evaluate a program and return a value.
  In some circumstances, it is impossible to continue evaluation.
  In these cases, raise the exception Eval
  For example, it is impossible to continue evaluation when:
  1) a program tries to apply a primitive operation to an object of the
     wrong type.  For instance, PrintInt can only print integers,
     not tuples or functions.  multiply can only multiply two
     integers.
  2) a program tries to apply a primitive operation to the wrong number
     of arguments.  Multiply, subtract and add all work on exactly 2
     arguments.
  3) a program tries to project (Proj) from something that is not
     a tuple (eg: it tries to project from an integer or a function).
  4) a program tries to do a call operation on something that is not a
     function
  5) a program tries to do an If test on something that is not an integer
*)

exception EvalError

val eval_prog : Absyn.prog -> value
