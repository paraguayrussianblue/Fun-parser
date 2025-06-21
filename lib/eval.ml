(* CSE322 Compiler Assignment 2 *)
(*
  Simple Fun Interpreter
  Original Authors: David Walker, Yitzhak Mandelbaum, Andrew Appel, etc.
*)

module A = Absyn
module H = Heap

exception EvalError

type value =
  | IntV of int
  | FunV of A.func
  | ExternV of A.id
  | TupleV of value list
  | LocV of H.loc

type external_fun = value -> value

let externals : (string * external_fun) list = [
  ("print_int", fun x -> match x with
    | IntV i -> print_int i; print_newline (); TupleV []
    | _ -> raise EvalError )]

type environment = value Symbol.table

let extern_env : environment = List.fold_left (fun env (name,body) ->
  Symbol.add (Symbol.symbol name) (ExternV (Symbol.symbol name)) env)
  Symbol.empty externals

let extern_funs : external_fun Symbol.table = List.fold_left (fun env (name,body) ->
  Symbol.add (Symbol.symbol name) body env)
  Symbol.empty externals

exception Eval

let rec eval_exp (topenv: environment)
	           (env: environment)
             (pos: Errormsg.pos * Errormsg.pos)
             (heap: value Heap.heap) =
  let eval_err s = Errormsg.error(pos, s); raise Eval in
  let rec ee x = match x with
    | A.Int(i) -> IntV i
    | A.Id(id) -> (match Symbol.find id env with
	    | Some x -> x
	    | None -> eval_err("Identifier " ^ Symbol.name id ^ " unknown."))
    | A.Op (oper, elist) -> (match (oper, List.map ee elist) with
      | (A.Add, [IntV i1; IntV i2]) -> IntV (i1 + i2)
      | (A.Sub, [IntV i1; IntV i2]) -> IntV (i1 - i2)
      | (A.Mul, [IntV i1; IntV i2]) -> IntV (i1 * i2)
      | (A.LT, [IntV i1; IntV i2]) -> IntV (if i1 < i2 then 1 else 0)
      | (A.Eq, [IntV i1; IntV i2]) -> IntV (if i1 = i2 then 1 else 0)
      | (A.Ref, [v]) -> (let l = Heap.fresh_loc heap in
                         (Heap.set_loc heap l v; LocV l))
      | (A.Get, [LocV l]) -> Heap.get_loc heap l
      | (A.Get, [_]) -> eval_err "Get of a nonlocation."
      | (A.Set, [LocV l; v]) -> (Heap.set_loc heap l v; TupleV[])
      | (A.Set, [_;_]) -> eval_err "Set of a nonlocation."
      | (_,_) -> eval_err "Invalid operator arguments.")
    | A.If(e1,e2,e3) -> (match ee e1 with
      | IntV 0 -> ee e3
      | IntV _ -> ee e2
 	    | _ -> eval_err "Expected an integer for test expression.")
    | A.While(e1,e2) -> (match ee e1 with
	    | IntV 0 -> TupleV[]
      | IntV _ -> let _ = ee e2 in ee (A.While (e1,e2))
 	    | _ -> eval_err "Expected an integer for test expression.")
    | A.Call (ef,ea) -> (
      let fun_val = ee ef in
      let arg_val = ee ea in
      match fun_val with
        | FunV (f_id,arg_id,a_tp,f_tp,f_body) ->
          eval_exp topenv (Symbol.add arg_id arg_val topenv) pos heap f_body
        | ExternV id -> (match Symbol.find id extern_funs with
          | Some f -> (try f arg_val with _ -> eval_err ("Error in "^ Symbol.name id))
          | None -> eval_err "Invalid external function.")
        | _ -> eval_err "Invalid function.")
      | A.Tuple es -> TupleV (List.map ee es)
      | A.Proj (i,e) -> (match ee e with
        | TupleV l -> (try List.nth l i with Failure s -> eval_err "No such element in tuple.")
        | _ -> eval_err "Expected tuple.")
      | A.Let (id,e1,e2) -> (
        let v1 = ee e1 in
        let env' = Symbol.add id v1 env in
        (eval_exp topenv env' pos heap) e2)
      | A.Constrain (e,tp) -> ee e
      | A.Pos (pos',e) -> eval_exp topenv env pos' heap e
  in ee

let add_fun env ((_,_), ((id,_,_,_,_) as f)) = Symbol.add id (FunV f) env

let eval_prog funs =
  let env = List.fold_left add_fun extern_env funs in
  let heap : value Heap.heap = Heap.empty()
  in eval_exp env env (0,0) heap
     (A.Call(A.Id(Symbol.symbol "main"), A.Int 0))
