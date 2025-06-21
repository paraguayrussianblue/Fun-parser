(* CSE322 Compiler Assignment 2 *)
(*
  Auxiliary Module for Printing a Fun Program
  Original Authors: David Walker, Yitzhak Mandelbaum, etc.
*)

module A = Absyn
module E = Eval

exception PrintError

type gmode =
  | GFlat  (* hgrp *)
  | GBreak (* vgrp *)
  | GAuto  (* agrp *)

type doc =
  | DocNil
  | DocCons  of doc * doc
  | DocText  of string
  | DocNest  of int * doc
  | DocBreak of string
  | DocGroup of gmode * doc

let empty       = DocNil
let text s      = DocText(s)
let nest i x    = DocNest(i,x)
let break       = DocBreak(" ")
let hgrp d      = DocGroup(GFlat, d)
let vgrp d      = DocGroup(GBreak,d)
let agrp d      = DocGroup(GAuto, d)

let (^^) x y    = DocCons(x,y)
let (^/) x y    = x ^^ break ^^ y

let strlen = String.length

type sdoc =
  | SNil
  | SText    of string * sdoc
  | SLine    of int    * sdoc    (* newline + spaces *)

let make_line i = "\n" ^ (String.make i ' ')

let sdoc_to_file oc doc =
  let pstr s = Stdlib.output_string oc s in
  let rec loop d = match d with
    | SNil -> ()
	  | SText(s,d) -> pstr s; loop d
	  | SLine(i,d) -> pstr (make_line i); loop d
  in loop doc

type mode =
  | Flat
  | Break

let rec fits w x = if w < 0 then false
  else match x with
    |  []                            -> true
	  | (i,m,DocNil)              :: z -> fits w z
	  | (i,m,DocCons(x,y))        :: z -> fits w ((i,m,x)::(i,m,y)::z)
	  | (i,m,DocNest(j,x))        :: z -> fits w ((i+j,m,x)::z)
	  | (i,m,DocText(s))          :: z -> fits (w - strlen s) z
	  | (i,Flat, DocBreak(s))     :: z -> fits (w - strlen s) z
	  | (i,Break,DocBreak(_))     :: z -> true
	  | (i,m,DocGroup(_,x))       :: z -> fits w ((i,Flat,x)::z)

let rec format w k x =
  match x with
	  | []                             -> SNil
    | (i,m,DocNil)              :: z -> format w k z
    | (i,m,DocCons(x,y))        :: z -> format w k ((i,m,x)::(i,m,y)::z)
    | (i,m,DocNest(j,x))        :: z -> format w k ((i+j,m,x)::z)
    | (i,m,DocText(s))          :: z -> SText(s ,format w (k + strlen s) z)
    | (i,Flat, DocBreak(s))     :: z -> SText(s ,format w (k + strlen s) z)
    | (i,Break,DocBreak(s))     :: z -> SLine(i,format w i z)
    | (i,m,DocGroup(GFlat ,x))  :: z -> format w k ((i,Flat ,x)::z)
    | (i,m,DocGroup(GBreak,x))  :: z -> format w k ((i,Break,x)::z)
    | (i,m,DocGroup(GAuto, x))  :: z -> if fits (w-k) ((i,Flat,x)::z)
					                              then format w k ((i,Flat ,x)::z)
					                              else format w k ((i,Break,x)::z)

let print_to_file oc w doc = sdoc_to_file oc (format w 0 [(0, Flat, agrp(doc))])

let seperate sep f xs =
  let rec loop acc l = match l with
    | [] -> acc
	  | [y] -> acc ^^ f y
	  | y::ys -> loop (acc ^^ f y ^^ sep) ys
  in loop empty xs

let comma_seperate f = seperate ((text ",") ^^ break) f

let unexpand = ref true

let id_to_doc id = text (Symbol.name id)
let int_to_doc i = text (Int.to_string i)
let loc_to_doc l = text ("l"^(Heap.loc_to_string l))

let prec_to_doc prec up_prec doc = if up_prec > prec
  then (agrp ((text "(") ^^ (nest 1 doc) ^^ (text ")")))
  else (agrp doc)

let rec tp_to_doc tp = match tp with
  | A.Inttp -> text "int"
  | A.Tupletp tps -> hgrp ((text "<")^^(nest 1 (comma_seperate tp_to_doc tps))^^(text ">"))
  | A.Arrowtp(tp1,tp2) ->
    agrp((tp_to_doc tp1) ^/ (nest 3 ((text "->") ^/ (tp_to_doc tp2))))
  | A.Reftp(tp) -> (tp_to_doc tp) ^/ (text "ref")

let prec_level = ref 0
let cur_prec () = !prec_level
let new_prec () = ((prec_level := !prec_level + 1);!prec_level)

let set_op    = (":=", new_prec())
let get_op    = ("!", new_prec())
let or_op     = ("||", new_prec())
let and_op    = ("&", new_prec())
let not_op    = ("not ", new_prec())
let lt_op     = ("<", new_prec())
let eq_op     = ("=", cur_prec())
let plus_op   = ("+", new_prec())
let sub_op    = ("-", cur_prec())
let mul_op    = ("*", new_prec())
let proj_op i = ("#" ^ (Int.to_string i) ^ " ", cur_prec ())
let uminus_op = ("-",cur_prec ())
let ref_op    = ("ref ", new_prec())

let rec is_let exp = match exp with
  | A.Let _ -> true
  | A.Pos(p,e) -> is_let(e)
  | _ -> false

let rec exp_to_doc up_prec e = match e with
	| A.Int i -> int_to_doc i
	| A.Id id -> id_to_doc id
	| A.Op (A.Add,[e1;e2]) -> binop_to_doc plus_op up_prec e1 e2
	| A.Op (A.Sub,[A.Int 0;e]) -> unop_to_doc uminus_op up_prec e
	| A.Op (A.Sub,[e1;e2]) -> binop_to_doc sub_op up_prec e1 e2
	| A.Op (A.Mul,[e1;e2]) -> binop_to_doc mul_op up_prec e1 e2
	| A.Op (A.LT,[e1;e2]) -> binop_to_doc lt_op up_prec e1 e2
	| A.Op (A.Eq,[e1;e2]) -> binop_to_doc eq_op up_prec e1 e2
	| A.Op (A.Set,[e1;e2]) -> binop_to_doc set_op up_prec e1 e2
  | A.Op (A.Ref,[e]) -> unop_to_doc ref_op up_prec e
  | A.Op (A.Get,[e]) -> unop_to_doc get_op up_prec e
  | A.Op _ -> text "UNKNOWN OP"
	| A.If (e1,e2,e3) ->(
	  let dflt (e1,e2,e3) = agrp
	    (((text "if ") ^^ (exp_to_doc 0 e1))
	     ^^(nest 2 ((text " then") ^/ (exp_to_doc 0 e2)))
	     ^/(nest 2 ((text "else") ^/ (exp_to_doc 0 e3)))) in
    if !unexpand then
	    match (e1,e2,e3) with
		    | (e1, A.If (e,A.Int 1, A.Int 0), A.Int 0) -> binop_to_doc and_op up_prec e1 e
	      | (e1, A.Int 1, A.If (e, A.Int 1, A.Int 0)) -> binop_to_doc or_op up_prec e1 e
	      | (e1, A.Int 0, A.Int 1) -> unop_to_doc not_op up_prec e1
	      | (e1,e2,e3) -> dflt(e1,e2,e3)
	  else dflt(e1,e2,e3))
  | A.While (e1,e2) -> agrp (
    ((text "while ")^^(exp_to_doc 0 e1))^^(nest 2 ((text " do")^/(exp_to_doc 0 e2))))
	| A.Call (ef,ea) ->  agrp
	  ((exp_to_doc 0 ef)^^(text "(")^^(exp_to_doc 0 ea)^^(text ")"))
	| A.Tuple es ->
	  agrp ((text "<")^^(comma_seperate (exp_to_doc 0) es)^^(text ">"))
	| A.Proj (i,e) -> unop_to_doc (proj_op i) up_prec e
	| A.Let (id,e1,e2) -> (vgrp
	  ((text "let ")^^(id_to_doc id)^^(text " = ")^^(exp_to_doc 0 e1)^^
		(if is_let e2 then ((text " in ")^/(exp_to_doc 0 e2))
		 else (nest 2 ((text " in ")^/(exp_to_doc 0 e2))))))
  | A.Constrain (e,t) ->
    agrp ((text "(")^^(exp_to_doc 0 e)^^(text ":")^^(tp_to_doc t)^^(text ")"))
	| A.Pos (pos,e) -> exp_to_doc up_prec e

and binop_to_doc (op_str,prec) up_prec e1 e2 =
  prec_to_doc prec up_prec
    ((exp_to_doc (prec) e1) ^/
	   (text op_str) ^/
	   (exp_to_doc (prec+1) e2))

and unop_to_doc (op_str,prec) up_prec e =
  prec_to_doc prec up_prec ((text op_str) ^^ (exp_to_doc (prec+1) e))

let fundec_to_doc (pos, (name,arg,arg_tp,fun_tp,e)) =
  agrp ((text "fun ")^^(id_to_doc name)^^
	      (agrp (text "(")^^(id_to_doc arg)^^(text ":")^^
        (tp_to_doc arg_tp)^^(text "):")^^(tp_to_doc fun_tp)^^
	      (agrp (nest 3 ((text " =") ^/ (exp_to_doc 0 e))))))

let prog_to_doc fs = vgrp ((seperate (break) fundec_to_doc fs) ^/ empty)

let print_prog p = let curr = !unexpand in
  unexpand := true;
  print_to_file Stdlib.stdout 74 (prog_to_doc p);
  unexpand := curr

let print_prog_as_is p = let curr = !unexpand in
  unexpand := false;
  print_to_file Stdlib.stdout 74 (prog_to_doc p);
  unexpand := curr

let print_fun f = print_to_file Stdlib.stdout 74 (fundec_to_doc ((0,0),f))

let print_exp e = print_to_file Stdlib.stdout 74 (exp_to_doc 0 e)

let rec value_to_doc value = match value with
  | E.IntV i -> int_to_doc i
  | E.FunV f -> fundec_to_doc ((0,0),f)
  | E.ExternV id -> id_to_doc id
  | E.TupleV vs -> agrp ((text "<") ^^ (comma_seperate value_to_doc vs) ^^ (text ">"))
  | E.LocV l -> loc_to_doc l

let print_value v = print_to_file Stdlib.stdout 74 (value_to_doc v)
