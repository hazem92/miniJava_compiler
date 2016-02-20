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
  | _ -> failwith "bug in get_infix_op : type error not catched"

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
  | _ -> failwith "bug in get_prefix_op : type error not catched"

(*get assign_op*)
let get_assign_op op x y env =
	match op, x, y  with
	| Assign, Name x, NullValue -> env#update_var_in_local_scope x NullValue
	| Assign, Name x, IntValue y -> env#update_var_in_local_scope x (IntValue y)
	| Assign, Name x, FloatValue y -> env#update_var_in_local_scope x ( FloatValue y)
	| Assign, Name x, StringValue y -> env#update_var_in_local_scope x ( StringValue y)
	| Assign, Name x, BoolValue y -> env#update_var_in_local_scope x ( BoolValue y )
	| Assign, Name x, CharValue y -> env#update_var_in_local_scope x ( CharValue y)
  | Assign, Name x, ClassValue y -> env#update_object_in_tas x y
  | Assign, Attr (x,z), NullValue -> (match x.edesc with Name s -> (env#update_att_in_tas s z NullValue));
	| Assign, Attr (x,z), IntValue y ->(match x.edesc with Name s-> (env#update_att_in_tas s z (IntValue y)))
	| Assign, Attr (x,z), FloatValue y ->(match x.edesc with Name s-> (env#update_att_in_tas s z ( FloatValue y)))
	| Assign, Attr (x,z), StringValue y ->(match x.edesc with Name s-> (env#update_att_in_tas s z ( StringValue y)))
	| Assign, Attr (x,z), BoolValue y ->(match x.edesc with Name s-> (env#update_att_in_tas s z ( BoolValue y)))
  | Assign, Attr (x,z), ClassValue y ->(match x.edesc with Name s-> (env#update_att_in_tas s z ( ClassValue y)))
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
  | _ -> failwith "bug in get_assign_op : type error not catched"

(*Eval experssions*)
let rec eval (exp:expression_desc) (env:environement) = match exp with
	(* case Val *)
	| Val (v) -> ( match v with
  	| String (s) -> StringValue (s)
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
	(*case Assign *)
  | AssignExp (e1,op,e2) -> (get_assign_op op) e1.edesc (eval e2.edesc env) env; NoValue;
  (*case Attr and Name*)
  | Attr (x,y) -> (match x.edesc with Name s -> env#get_att_value_from_tas s y)
  | Name (s) -> ( if (Hashtbl.mem env#local_scope s) then (
                let v=Hashtbl.find env#local_scope s in
                v._value
                 ) else (
                raise (RunTimeError ("this object "^s^" was not declared in this scope"))
                  )
                )
  (*case New*)
  | New (None,n,al) -> (
    let name = List.nth n ((List.length n) -1) in
    if not (Hashtbl.mem (env#get_classes) name ) then
      raise (RunTimeError ("this Class "^name^" was not defined")) ;
    let c = Hashtbl.find env#get_classes name in
    let attrs = c.def_attributes in
    let attributes:(string,Environement.value) Hashtbl.t= Hashtbl.create 0 in
    let f = fun x y -> Hashtbl.add attributes x (eval y.edesc env)  in
    Hashtbl.iter f attrs;
    let tob = {_name = "default";_class = name; attributes = attributes } in
    ClassValue ( Hashtbl.length env#get_tas ) )

(* execute var declaration*)
let eval_var_dec (t:Type.t) (n:string) (e:expression option) (env:environement) =
  match t with
    |Primitive (p) ->(
      match e with
        | None -> env#add_var_to_scope n ;
        | Some ex -> (
          env#add_var_to_scope n ;
          let value = eval ex.edesc env in
          (get_assign_op) Assign (Name n) value env ;
          ))
    |Ref (r) -> (
      let name = r.tid in
    if not (Hashtbl.mem (env#get_classes) name ) then
      raise (RunTimeError ("this Class "^name^" was not defined")) ;
    let c = Hashtbl.find env#get_classes name in
    let attrs = c.def_attributes in
    let attributes:(string,Environement.value) Hashtbl.t= Hashtbl.create 0 in
    let f = (fun x y -> Hashtbl.add attributes x (eval y.edesc env))  in
    Hashtbl.iter f attrs;
    let tob = {_name = "default";_class = name; attributes = attributes } in
    env#add_obj_in_tas tob;
    env#add_var_to_scope n ;
    env#update_var_in_local_scope n (ClassValue ( Hashtbl.length env#get_tas ));
    match e with
      |None -> ()
      |Some exp -> (let value = eval exp.edesc env in
      (get_assign_op) Assign (Name n) value env ;))
(*execute statements*)
let eval_statement (s:statement) (env:environement) =
  match s with
    | Expr (exp) -> eval exp.edesc env ;();
    | VarDecl (l) -> (
        let f = fun x -> (
          match x with
            | (t:Type.t),(n:string),(e:expression option) -> eval_var_dec t n e env ;
            | _ -> raise (RunTimeError ("can not instantiate "))
          ) in
        List.iter f l )

let rec eval_statement_list l (env:environement) =
  match l with
    | [] -> () ;
    | statement::l -> eval_statement statement env ; eval_statement_list l env;

(* execute main method *)
and execute_main_method (env:environement) =
  let main_method_body = env#get_main_method_body in
  env#create_new_scope ;
  eval_statement_list main_method_body env ;
  print_int (Hashtbl.length env#get_tas) ;
  if ((String.compare (env#string_of_local_scope) "") < 0 ) then (print_string "\n empty local scope" ;)
    else print_string env#string_of_local_scope ;
  env#exit_scope ;

and execute_method (ref_o) (n_m:string) (pl: value list) (env:environement) =
  env#create_new_scope ;
  let tob = env#get_obj_from_tas ref_o in
  let cname = tob._class in
  let m_body = env#get_method_body_from_gmethods cname n_m  in
  let arg_list = m_body.margstype in
  let scope = env#local_scope in
  let f = (fun key value -> Hashtbl.add scope key.pident value) in
  list.iter2 f arg_list pl ;
  Hashtbl.add scope "this" ref_o ;
  env#set_local_scope scope ;
  eval_statement_list m_body env ;
  print_int (Hashtbl.length env#get_tas) ;
  if ((String.compare (env#string_of_local_scope) "") < 0 ) then (print_string "\n empty local scope" ;)
    else print_string env#string_of_local_scope ;
  env#exit_scope ;
