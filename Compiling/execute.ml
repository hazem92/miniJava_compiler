open AST
open Environement
open Exceptions


(*add class definition to enviroment from ast *)
let rec gather_toplevel l env = match l with
  |elem::tl -> (match elem.info with
  |Class(x) -> env#add_class elem ; gather_toplevel tl env);
	| _ ::tl -> gather_toplevel tl env
	| [] -> ()

(* get infix_op *)
let get_infix_op op x y =
  match op, x, y with
  | Op_add, IntValue x, IntValue y -> IntValue (x + y)
	| Op_add, FloatValue x, FloatValue y -> FloatValue (x +. y)
	| Op_add, StringValue x, StringValue y -> StringValue (x ^ y)
	| Op_sub, IntValue x, IntValue y -> IntValue (x - y)
	| Op_sub, FloatValue x, FloatValue y -> FloatValue (x -. y)
	| Op_mul, IntValue x, IntValue y -> IntValue (x * y)
	| Op_mul, FloatValue x, FloatValue y -> FloatValue (x *. y)
	| Op_div, IntValue x, IntValue y -> IntValue (x / y)
	| Op_div, FloatValue x, FloatValue y -> FloatValue (x /. y)
	| Op_mod, IntValue x, IntValue y -> IntValue (x mod y)
  | Op_cor, BoolValue x, BoolValue y -> BoolValue (x || y)
	| Op_cand, BoolValue x, BoolValue y -> BoolValue (x && y)
	| Op_eq, BoolValue x, BoolValue y -> BoolValue (x == y)
	| Op_eq, IntValue x, IntValue y -> BoolValue (x == y)
	| Op_eq, FloatValue x, FloatValue y -> BoolValue (x == y)
	| Op_eq, StringValue x, StringValue y -> BoolValue (x == y)
	| Op_ne, BoolValue x, BoolValue y -> BoolValue (x != y)
	| Op_ne, IntValue x, IntValue y -> BoolValue (x != y)
	| Op_ne, FloatValue x, FloatValue y -> BoolValue (x != y)
	| Op_ne, StringValue x, StringValue y -> BoolValue (x != y)
	| Op_gt, IntValue x, IntValue y -> BoolValue (x > y)
	| Op_gt, FloatValue x, FloatValue y -> BoolValue (x > y)
	| Op_lt, IntValue x, IntValue y -> BoolValue (x < y)
	| Op_lt, FloatValue x, FloatValue y -> BoolValue (x < y)
	| Op_ge, IntValue x, IntValue y -> BoolValue (x >= y)
	| Op_ge, FloatValue x, FloatValue y -> BoolValue (x >= y)
	| Op_le, IntValue x, IntValue y -> BoolValue (x <= y)
	| Op_le, FloatValue x, FloatValue y -> BoolValue (x <= y)
	| Op_or,_,_ -> raise (RunTimeError (" Op_or is not yet supported "))
	| Op_and,_,_ -> raise (RunTimeError (" Op_and is not yet supported "))
	| Op_xor,_,_ -> raise (RunTimeError (" Op_xor is not yet supported "))
	| Op_shr,_,_ -> raise (RunTimeError (" Op_shr is not yet supported "))
	| Op_shrr,_,_ -> raise (RunTimeError (" Op_shrr is not yet supported "))
  | _ -> failwith "bug:type error not catched"

	(* get postfix_op *)
let get_postfix_op op x  =
  match op,x  with
	| Incr,IntValue x -> IntValue (x+1)
  | Decr,IntValue x -> IntValue (x-1)

(* get postfix_op *)
let get_prefix_op op x =
	match op,x  with
	| Op_not, BoolValue x -> BoolValue (not x)
	| Op_neg, IntValue x -> IntValue (-x)
	| Op_neg, FloatValue x -> FloatValue (-.x)
	| Op_incr,IntValue x -> IntValue (x+1)
	| Op_decr,IntValue x -> IntValue (x-1)
	| Op_bnot,_ -> raise (RunTimeError (" Op_bnot is not yet supported "))
	| Op_plus,_ -> raise (RunTimeError (" Op_plus is not yet supported "))

(*get assign_op*)
let get_assign_op op x y env =
	match op x y  with
	| Assign, Name x, NullValue -> env#update_var_in_local_scope x NullValue
	| Assign, Name x, IntValue y -> env#update_var_in_local_scope x (IntValue y)
	| Assign, Name x, FloatValue y -> env#update_var_in_local_scope x ( FloatValue y)
	| Assign, Name x, StringValue y -> env#update_var_in_local_scope x ( StringValue y)
	| Assign, Name x, BoolValue y -> env#update_var_in_local_scope x ( BoolValue y )
	| Assign, Name x, CharValue y -> env#update_var_in_local_scope x ( CharValue y)
(*
	| Ass_add
	| Ass_sub
	| Ass_mul
	| Ass_div
	| Ass_mod
	| Ass_shl
	| Ass_shr
	| Ass_shrr
	| Ass_and
	| Ass_xor
	| Ass_or *)

(*Eval experssions*)
let rec eval (exp:expression_desc) env = match exp with
	(* case Val *)
	| Val (v) -> ( match v with
  	| String (s) -> IntValue (int_of_string s)
  	| Int (s) -> IntValue (int_of_string s)
  	| Float (s) -> FloatValue (float_of_string s)
  	| Char (s)  -> ( match s with
	 						 	| None -> NoValue
							 	| Some c -> CharValue (c) )
  	| Boolean (s) -> BoolValue (s)
  	| Null  -> NullValue )
	(* case Op *)
	| Op (e1,op,e2) -> (get_infix_op op) (eval e1.edesc env) (eval e2.edesc env)
	(*case Post and Pre*)
	| Post (e,op) -> ( get_postfix_op op ) (eval e.edesc env)
  | Pre (op,e) -> ( get_prefix_op op ) (eval e.edesc env)
	(*case New*)
