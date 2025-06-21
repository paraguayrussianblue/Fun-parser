(* CSE322 Compiler Assignment 2 *)

open As2

let compile filename =
  try
    let file = Stdlib.open_in filename in
    let lexbuf = Lexing.from_channel file in
    let _ = Errormsg.reset filename in
    let program = Funpar.prog Funlex.initial lexbuf in
    let _ = print_endline "Program:" in
    let _ = Printer.print_prog program in
    let _ = print_endline "Running Program..." in
    let value = Eval.eval_prog program in
    let _ = print_endline "Program Output:" in
    let _ = Printer.print_value value in
    print_newline (); print_newline ()
  with Errormsg.Error -> print_endline "Prasing failed."

let () = compile "data/all.fun"
let () = compile "data/test.fun"
