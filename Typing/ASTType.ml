open AST
open Type
  
let get_methods_by_name c =
  let methods = Hashtbl.create 10 in
  List.iter ( fun m ->
    Hashtbl.add methods m.mname m
  ) c.cmethods;
  methods

let get_methods_by_class program =
  let classes = Hashtbl.create 10 in
  List.iter ( fun t ->
    match t.info with
      | Class c -> Hashtbl.add classes t.id (get_methods_by_name c)
  ) program.type_list;
  classes

exception Invalid_typing of string

let check_absolute_type_match env = function
  | t1, t2 ->
    if (t1 = t2) then () else raise(Exceptions.TypingError("Incompatible types: "^(stringOf t1)^" with "^(stringOf t2))) 

let rec check_ref_type env = function
  | (Ref r1 as t1), (Ref r2 as t2) ->
    if (r2.tid = "null") then () else
    (try check_absolute_type_match env (t1, t2) with Exceptions.TypingError s ->
      let parent_class = (env#get_global_env#get_class r2.tid) in
      let parent_type = ASTTypeSolver.filtered_type env (Ref (match parent_class.info with Class c -> c.cparent)) in
      print_endline (stringOf parent_type);
      (try check_ref_type env (t1, parent_type) with Exceptions.TypingError s2 ->
        raise(Exceptions.TypingError(s))
      )
    )

(* Checks valid implicit casts from t2 to t1 *)
let check_type_match env t1 t2 =
  match ASTTypeSolver.filtered_type env t1, ASTTypeSolver.filtered_type env t2 with
    | Primitive Char, Primitive Char
    | Primitive Int, Primitive (Char|Int)
    | Primitive Float, Primitive (Char|Int|Float) -> ()
    | (Ref r1 as t1), (Ref r2 as t2) -> check_ref_type env (t1, t2)
    | t1, t2 -> check_absolute_type_match env (t1, t2)
    
let check_explicit_cast_type_match env t1 t2 =
  match t1, t2 with
    | Primitive (Char|Int|Float), Primitive (Char|Int|Float) -> ()
    | t1, t2 -> (check_type_match env t1 t2)

let check_bidirectional_type_match env t1 t2 =
  try
    check_type_match env t2 t1
  with Invalid_typing s ->
    check_type_match env t1 t2

let validate_expression_typing env = function
  | AssignExp(e1,op,e2) -> (match e1.etype, e2.etype with Some(t), Some(t2) -> check_type_match env t t2 )
  | Cast (t, e) -> (match e.etype with Some(t2) -> check_explicit_cast_type_match env t t2)
  | CondOp(_, e1, e2) -> (match e1.etype, e2.etype with Some(t), Some(t2) -> check_bidirectional_type_match env t t2)
  | _ -> ()

let rec type_expression_composants env = function
  | Op(e1, _, e2)
  | AssignExp(e1, _, e2) ->
    type_expression e1 env; 
    type_expression e2 env
  | CondOp(e1, e2, e3) ->
    List.iter (fun e -> type_expression e env) [e1; e2; e3]
  | Post(e,_)
  | Pre(_,e)
  | Attr (e, _)
  | Cast (_, e) ->
    type_expression e env
  | Call(eo, id, params) ->
    (match eo with Some(e) -> type_expression e env | None -> ());
    List.iter (fun p -> type_expression p env) params
  | New(_, _, params) ->
    List.iter (fun p -> type_expression p env) params
  | _ -> ()
  
and type_expression e env =
  (type_expression_composants env) e.edesc;
  validate_expression_typing env e.edesc;
  print_string ("Typing expression: "^string_of_expression e);
  let etype = (ASTTypeSolver.type_of_expression_desc env) e.edesc in
  e.etype <- Some(etype);
  print_endline ("<- "^stringOf etype)

(*type expression_desc = 
  | NewArray of Type.t * (expression option) list * expression option
  | ArrayInit of expression list
  | Array of expression * (expression option) list
  | CondOp of expression * expression * expression
  | Type of Type.t
  | ClassOf of Type.t
  | Instanceof of expression * Type.t
  | VoidClass
  | QN of string list*)
  
let type_declaration d env =
  List.iter(fun (t, id, init) ->
    env#add_var id t;
    match init with
      | Some(e) -> type_expression e env; (match e.etype with Some(t2) -> (check_type_match env t t2) | None -> ())
      | None -> ()
  ) d

let rec type_statement env = function
  | Expr e
  | Throw e -> type_expression e env
  | Block b -> List.iter (type_statement env) b
  | VarDecl d -> type_declaration d env
  | If (e, s, None)
  | While (e, s) -> type_expression e env; (type_statement env) s
  | For _ as f -> (type_for_statement env) f
  | If (e, s1, Some(s2)) -> let inner_env = env#get_inner_env in type_expression e inner_env; (List.iter (type_statement inner_env) [s1;s2])
  | Try (sl, catch_list, sl2) -> ()
  | Return Some(e) ->
    type_expression e env;
    (match env#get_return_type, e.etype with
      | Some(t1), Some(t2) -> (check_type_match env t1 t2)
      | None, Some(t2) -> raise(Exceptions.TypingError("Incompatible types: None with "^(stringOf t2)))
    )
  | Return None
  | Nop
  | _ -> ()
  
and type_for_statement env = function
  | For (init_el1, e, post_el2, s) -> (
    List.iter ( fun a -> (match a with
      | Some(t), id, e -> (type_statement env) (VarDecl [(t,id,e)])
      | None, id, Some(e) -> type_expression e env; (match e.etype with Some(t) -> check_type_match env ((ASTTypeSolver.type_of_name env) id) t)
      | _ -> ()
    ) ) init_el1;
    (match e with Some(e) -> type_expression e env | None -> ());
    List.iter (fun e -> type_expression e env) post_el2;
    type_statement env s
  )

let type_method global_env localclass package m =
  let env = new ASTTypeEnv.function_scoped_var_type_env (Some(m.mreturntype)) global_env localclass package () in
  List.iter (fun arg -> env#add_var arg.pident arg.ptype) m.margstype;
  List.iter (type_statement env) m.mbody

let type_const global_env localclass package c =
  let env = new ASTTypeEnv.function_scoped_var_type_env None global_env localclass package () in
  List.iter (fun arg -> env#add_var arg.pident arg.ptype) c.cargstype;
  List.iter (type_statement env) c.cbody

let type_attribute global_env localclass package a =
  let env = new ASTTypeEnv.function_scoped_var_type_env None global_env localclass package () in
  match a.adefault with
    | None -> ()
    | Some e -> type_expression e env

let type_class global_env c classname package =
  List.iter (type_attribute global_env classname package) c.cattributes;  
  List.iter (type_method global_env classname package) c.cmethods;
  List.iter (type_const global_env classname package) c.cconsts

let rec type_entity global_env package entity =
  match entity.info with
    | Class c -> type_class global_env c entity.id package; List.iter (type_entity global_env package) c.ctypes
    | Inter -> () (* Interfaces *)

let type_program global_env program =
  match program.package with
    | None -> ()
    | Some pack -> List.iter (type_entity global_env pack) program.type_list
