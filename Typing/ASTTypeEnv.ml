class function_var_type_env
  (return_type : (Type.t option))
  (global_env : (TypeEnv.global_env))
  (local_class : string)
  (package : string list) = 
  object (self)
    val vars = Hashtbl.create 0;
    method get_var v = Hashtbl.find vars v
    method add_var (id:string) (t:Type.t) = Hashtbl.add vars id t
    method get_return_type = return_type
    method get_package = package
    method get_local_class = local_class
    method get_local_class_structure = global_env#get_class local_class
    method get_local_class_method id = global_env#get_method id local_class
    method get_local_class_attribute id = global_env#get_attribute id local_class
    method get_global_env = global_env
  end

(* get_inner_env creates a child local environment for variable declaration in an inner block *)  
class function_scoped_var_type_env
  returntype
  globalenv
  localclass
  package
  ?p:(parent_env=None)
  () =
  object (self)
    val env = new function_var_type_env returntype globalenv localclass package
    method add_var (id:string) (t:Type.t) = env#add_var id t
    method get_var v = 
      ( try globalenv#get_class v; Type.Ref { tpath = package ; tid = v } with Exceptions.TypingError s -> (* Check if it's a class reference *)
        try env#get_var v with Not_found -> (* Search in the current block scope *)
        (
          match parent_env with
            | Some(e) -> e#get_var v (* Well, search in the outer block scope *)
            | None -> (env#get_global_env#get_attribute v env#get_local_class).atype (* The last possibility: search in the class attributes *)
        )
      )
    method get_return_type = env#get_return_type
    method get_package = env#get_package
    method get_local_class = env#get_local_class
    method get_local_class_structure = env#get_local_class_structure
    method get_local_class_method id = env#get_local_class_method id
    method get_local_class_attribute id = env#get_local_class_attribute id
    method get_global_env = env#get_global_env
    method get_inner_env = new function_scoped_var_type_env returntype globalenv localclass package ~p:(Some(self :> function_scoped_var_type_env)) ()
  end
