open AST
open Type

let type_of_value = function
  | AST.Int _ -> Primitive Int
  | String _ -> Ref { tpath = [] ; tid = "String" }
  | AST.Float _ -> Primitive Float
  | AST.Char _ -> Primitive Char
  | Null -> Ref { tpath = [] ; tid = "null" }
  | AST.Boolean _ -> Primitive Boolean

let type_of_intarith_infixop = function
  | Primitive Char, Primitive Char -> Primitive Char
  | Primitive (Int|Char), Primitive (Int|Char) -> Primitive Int
  | _ -> Ref { tpath = [] ; tid = "INVALID_INT_BINOP" }
  
let type_of_arith_infixop = function
  | (Primitive (Int|Char) as t1), (Primitive (Int|Char) as t2) -> type_of_intarith_infixop(t1, t2)
  | Primitive (Float|Int|Char), Primitive (Float|Int|Char) -> Primitive Float
  | _ -> Ref { tpath = [] ; tid = "INVALID_ARITH_BINOP" }

let type_of_condop = function
  | (_, Some(Primitive _ as t1), Some(Primitive _ as t2)) -> type_of_arith_infixop(t1, t2)
  | (_, Some(t1), _) -> t1

let type_of_binop = function
    | (Op_add|Op_sub|Op_mul|Op_div|Op_mod), Some(t1), Some(t2) -> type_of_arith_infixop(t1, t2)
    | (Op_xor|Op_or|Op_and|Op_shl|Op_shr|Op_shrr), Some(t1), Some(t2) -> type_of_intarith_infixop(t1, t2)
    | (Op_cand|Op_cor), Some(Primitive Boolean), Some(Primitive Boolean)
    | (Op_eq|Op_ne|Op_gt|Op_lt|Op_ge|Op_le), Some(Primitive (Float|Int|Char)), Some(Primitive (Float|Int|Char))
    | (Op_eq|Op_ne), Some(Ref _), Some(Ref _) -> Primitive Boolean
    | _ -> Ref { tpath = [] ; tid = "INVALID_BINOP" }
  
let type_of_postop = function
  | op, Some(Primitive (Float|Int|Char) as t) -> t
  | _ -> Ref { tpath = [] ; tid = "INVALID_POSTOP" }

let type_of_preop = function
  | Op_not, Some(Primitive Boolean as t)
  | Op_bnot, Some(Primitive (Int|Char) as t)
  | (Op_neg|Op_incr|Op_decr|Op_plus), Some(Primitive (Float|Int|Char) as t) -> t
  | _ -> Ref { tpath = [] ; tid = "INVALID_PREOP" }

let type_of_ref env r =
  match r.tpath with
  | [] -> (try env#get_global_env#get_class r.tid; Ref { tpath = env#get_package ; tid = r.tid } with Exceptions.TypingError s -> Ref r)
  | _ ->  Ref r

let type_of_name env = function
  | "super" -> (match (env#get_local_class_structure).info with Class c -> Ref c.cparent)
  | "this" -> Ref { tpath = [] ; tid = env#get_local_class }
  | s -> env#get_var s

let type_of_call env = function
  | Call (None, (("this"|"super") as id), params) -> (type_of_name env) id
  | Call (None, id, params) -> (env#get_local_class_method id).mreturntype
  | Call (Some(e), id, params) ->
    (match e.edesc with
      | Name("this") -> (env#get_local_class_method id).mreturntype
      | _ ->
        (match e.etype with
          | Some(Ref r) -> (env#get_global_env#get_method id r.tid).mreturntype
          | _ -> Ref { tpath = [] ; tid = "INVALID_CALL" }
        )
    )
  | Call _ -> Ref { tpath = [] ; tid = "UNDEFINED_CALL" }

let type_of_expression_desc env = function  
    | Val v -> type_of_value v
    | Op(e1,op,e2) -> type_of_binop (op, e1.etype, e2.etype)
    | Post(e,op) -> type_of_postop (op, e.etype)
    | Pre(op,e) -> type_of_preop (op, e.etype)
    | CondOp(c, e1, e2) -> type_of_condop (c, e1.etype, e2.etype)
    | AssignExp(_, _, e) -> (match e.etype with Some(t) -> t)
    | Attr (e, id) -> (match e.etype with Some Ref r -> (env#get_global_env#get_attribute id r.tid).atype)
    | Call _ as c -> (type_of_call env) c
    | Name m -> (type_of_name env) m
    | Cast (t, e) -> t
    | Type t
    | ClassOf t -> t
    | New (_, path, _) -> Ref { tpath = List.rev(List.tl (List.rev(path))) ; tid = List.hd(List.rev (path)) }
    | _ -> Ref { tpath = [] ; tid = "UNDEFINED" }
    
let filtered_type env = function
    | Ref r -> (type_of_ref env r) (* Explicit package *)
    | t -> t
