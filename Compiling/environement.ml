open Type
open AST
open Exceptions

(* Definitions of used Data structures in environement
   Class will have a list of string of its defined methods, the implementation of these methods
   will be contained in the global_methodes attribute
*)
(* Definition of the value type after execution *)
type value =
	| NullValue
	| IntValue of int
	| FloatValue of float
	| StringValue of string
	| BoolValue of bool
  | CharValue of char
	| ClassValue of int
	| NoValue

type class_data = {mutable def_attributes:(string,AST.expression) Hashtbl.t; mutable def_methods:string list ; mutable parent:string}
type method_data = astmethod
type scope_Hashtbl_contenant = {mutable _value : value}
(*tas*)
type tObject = {_name:string;_class:string; mutable attributes:(string, value) Hashtbl.t}
type ttas = (int,tObject) Hashtbl.t

(*Definition of the class environement *)
class environement =
  object (self)

  val global_methodes = Hashtbl.create 0 ;
  val tas:ttas = Hashtbl.create 0;
  val  classes = (let c = Hashtbl.create 0 in

(*Definition of particular classes *)
  let object_class = {def_attributes = Hashtbl.create 0; def_methods =  ["Object_getClass";"Object_equals";"Object_toString"]; parent=""} in
  Hashtbl.add c "Object" object_class;
  let string_class = {def_attributes = Hashtbl.create 0; def_methods =  ["String_concat";"String_contains";"String_equals";"String_isEmpty";"String_length";"String_split"]; parent="Object"} in
  Hashtbl.add c "String" string_class;
  let integer_class = {def_attributes = Hashtbl.create 0; def_methods =  ["Integer_equals";"Integer_toString"]; parent="Object"} in
  Hashtbl.add c "Integer" integer_class;
  let boolean_class = {def_attributes = Hashtbl.create 0; def_methods =  ["Boolean_compare";"Boolean_toString"]; parent="Object"} in
  Hashtbl.add c "Boolean" boolean_class;
  c;)

(* begin Definition of scope and useful methods*)

(* scopes are a list of Hashtbl*)
val mutable scopes : ((string,scope_Hashtbl_contenant) Hashtbl.t) list = [Hashtbl.create 0] ;
val mutable main_class = {parent = "Object"; def_attributes = Hashtbl.create 0; def_methods = [] } ;

method set_main_class_env =
	main_class <- Hashtbl.find classes "Main"
method get_main_method_body =
 	let main = Hashtbl.find global_methodes "Main_main" in
	print_string "main method in Main class found" ;
	print_newline;
	main.mbody
method add_var_to_scope (v:string) =
	if Hashtbl.mem self#local_scope v then
		raise (RunTimeError ("Variable " ^v^ " already declared in this scope"))
	else
		( let head = self#local_scope in
			Hashtbl.add head v { _value = NoValue} ;
			let tail = self#tail_scopes in
			let new_scopes = head::tail in
			scopes <- new_scopes ;
		)
method get_classes =
	classes
method get_tas =
	tas
method add_obj_in_tas (obj:tObject)=
	Hashtbl.add tas ((Hashtbl.length tas) +1) obj
method local_scope =
  if ((List.length scopes) > 1) then
    List.hd scopes
  else
    raise (RunTimeError("Something went wrong with scopes"))
method tail_scopes =
  if ((List.length scopes) > 1) then
    List.tl scopes
  else
    raise (RunTimeError("Something went wrong with scopes"))
method clear_scopes =
  (*assert (List.length scopes == 1);*)
  scopes <- [Hashtbl.create 0]
method create_new_scope =
  scopes <- (Hashtbl.create 0) :: scopes
method exit_scope =
  (*assert ((List.length scopes) > 1);*)
  scopes <- List.tl scopes

method add_attribute_to_local_scope (a:astattribute) =
  if Hashtbl.mem self#local_scope a.aname then
	   raise (CompilingError ("Variable " ^ a.aname ^ " already declared in local scope"))
	else
	  () (*Hashtbl.add self#local_scope a.aname {_type = (Typing.string_to_type a._type); _value = NoValue}*)
method add_constant_to_local_scope (c:astconst) =
  if Hashtbl.mem self#local_scope c.cname then
    raise (CompilingError ("Constant " ^ c.cname ^ " already declared in local scope"))
  else
    () (*Hashtbl.add self#local_scope v.name {_type = (Typing.string_to_type v._type); value = NoValue}*)
method add_arg_to_local_scope (a:argument) =
  if Hashtbl.mem self#local_scope a.pident then
    raise (CompilingError ("Param " ^ a.pident ^ " already declared in local scope"))
  else
    () (*Hashtbl.add self#local_scope v.pident {_type = (Typing.string_to_type v._type); value = NoValue}*)
(*method match_param_to_arg*)

method update_var_in_local_scope (s:string) (v:value) =
  if (Hashtbl.mem self#local_scope s) then (
		let head = self#local_scope in
		Hashtbl.replace head s { _value = v} ;
		let tail = self#tail_scopes in
		let new_scopes = head::tail in
		scopes <- new_scopes ;
		) else (
    raise (RunTimeError ("this variable "^s^" was not declared in this scope")) )

method update_att_in_tas (n:string) (s:string) (v:value) =
  if (Hashtbl.mem self#local_scope n) then (
    let tmp_i = Hashtbl.find self#local_scope n in
      match tmp_i._value with
      | ClassValue i -> (
        let tmp_ob = Hashtbl.find tas i in
        if (Hashtbl.mem tmp_ob.attributes s) then (
					(* Depending on type of v if it's a primitive we replace, if it's ClassValue
					 we make a copy of attributes between the attribute object and the attributes
					 of object whose int (ref) in tas was passed in parameter value *)
					 match v with
					  	| ClassValue a -> (
								 let tmp_v = Hashtbl.find tmp_ob.attributes s in
								 match tmp_v with
 										| ClassValue j -> (
 											let obj_start = Hashtbl.find tas j in
 											let obj_end = Hashtbl.find tas a in
 											obj_start.attributes <- obj_end.attributes
 									)
 								| _ -> raise (RunTimeError ("something not cool happened"))
								)
							| _ ->	Hashtbl.replace tmp_ob.attributes s v
           ) else (
            raise (RunTimeError ("this object "^n^" does not have attribute "^s))))
     | _ -> raise (RunTimeError ("something not cool happened"))
     )
  else (
    raise (RunTimeError ("this object "^n^" was not declared in this scope"))
    )

method get_att_value_from_tas (n:string) (s:string)  =
  if (Hashtbl.mem self#local_scope n) then (
    let tmp_i = Hashtbl.find self#local_scope n in
      match tmp_i._value with
      | ClassValue i -> (
        let tmp_ob = Hashtbl.find tas i in
        if (Hashtbl.mem tmp_ob.attributes s) then (
          	Hashtbl.find tmp_ob.attributes s  ) else (
            raise (RunTimeError ("this object "^n^" does not have attribute "^s))))
     | _ -> raise (RunTimeError ("something not cool happened"))
     )
  else (
    raise (RunTimeError ("this object "^n^" was not declared in this scope"))
    )

method update_object_in_tas (s:string) (n:int) =
	if (Hashtbl.mem self#local_scope s) then (
		let tmp_s = Hashtbl.find self#local_scope s in
		match tmp_s._value with
		| ClassValue i -> (
			let obj_start = Hashtbl.find tas n in
			let obj_end = Hashtbl.find tas n in
			obj_start.attributes <- obj_end.attributes
			)
		| _ -> raise (RunTimeError ("something not cool happened"))
		)
		else (
			raise (RunTimeError ("this object "^s^" was not declared in this scope")))

(*end*)




(*avoid having interface instance +++todo+++ *)
(*  Compiling Class.. adding class to the enviroment
    Check if Parent is defined and no other class is defined having the same name
    Building the table of methods
 *)

  method add_class (cl : asttype)  =  match cl.info with
  |Class classe ->
    (*if ( not (Hashtbl.mem classes classe.cparent.tid)) then
    raise (CompilingError ("The parent Class of " ^ cl.id ^ " is not known"));
    if (Hashtbl.mem classes cl.id) then
    raise (CompilingError ("Class " ^ cl.id ^ " is already defined")); *)
    let class_data = {parent = classe.cparent.tid; def_attributes = Hashtbl.create 0; def_methods = [] } in

      (*print_endline ("nom parent " ^ classe.cparent.tid) ;
      print_endline ("nom classe " ^ cl.id );
      print_endline "class added successfully ";
      print_endline "" ;
      print_int (Hashtbl.length classes) ;
      print_endline "" ;*)

    (*Adding methods of class to global_methodes
      and verify if parent class methods are not defined and add them
      to the list of this class methods definition
    *)
    let parent_class = Hashtbl.find classes classe.cparent.tid in

    List.map
    (fun x ->( if not (List.mem (cl.id^"_"^x.mname) class_data.def_methods ) then
    (class_data.def_methods <- ((cl.id^"_"^x.mname)::class_data.def_methods))
    else
    (raise (CompilingError ("in the Class " ^ cl.id ^ " the method "^x.mname^" was declared more than one time")); ) ))
    classe.cmethods ;
    List.map (fun x -> Hashtbl.add global_methodes (cl.id^"_"^x.mname) x ) classe.cmethods ;

    let method_list = classe.cmethods in
    (* Extract the list of methods names from the list of the methods of the class &
    Add the parent class name as a prefix to each method name in order to simpify the
    test in the 'verify_methods' function *)

    let method_remove_parent_name = function
    | x -> String.sub x ((String.length classe.cparent.tid) + 1) ((String.length x) - (String.length classe.cparent.tid+ 1) )
    in

    let rec method_list_name = function
    | [] -> []
    | x :: l ->(classe.cparent.tid^"_"^x.mname)::(method_list_name l) in
    let list_name_method = method_list_name method_list in

    (*add the methods of parent class to the list of methods of this class
    and checks in case of redifinition that parnt class and this class has the same return type
    and argument list *)

    let rec compare_args_list_types = function
    | [],[] -> true
    | _,[] -> false
    | [],_ -> false
    | l1,l2 -> let (l11,l12) = List.partition (fun x -> ( x.ptype = (List.hd l1).ptype ) ) l1 in
               let (l21,l22) = List.partition (fun x -> ( x.ptype = (List.hd l1).ptype ) ) l2 in
                if not (List.length l11 = List.length l11) then
                  false
                else
                compare_args_list_types (l12,l22) ;
    in

    let verify_methods  = function
      | x ->
      (if not (List.mem x list_name_method) then
       (class_data.def_methods <- x::class_data.def_methods ;) else
       (if not (
         (*we compare the definition of methods in global_methodes : same return *)
         ((Hashtbl.find global_methodes x).mreturntype =
            ((Hashtbl.find global_methodes (cl.id^"_"^(method_remove_parent_name x))).mreturntype))
         &&
         (*we compare the definition of methods in global_methodes : same arg list types *)
         (compare_args_list_types (((Hashtbl.find global_methodes x).margstype),
              ((Hashtbl.find global_methodes (cl.id^"_"^(method_remove_parent_name x))).margstype)))
         ) then
       (raise (CompilingError ("Incorrect redifinition of the method " ^ method_remove_parent_name x));)
       ))
    in
    List.map verify_methods parent_class.def_methods;

    (*let rec print_list = function
    [] -> print_string cl.id ; print_endline ""
    | e::l -> print_string e ; print_endline "" ; print_list l
    in*)
    let rec add_attributes =function
      | [] -> ()
      | att::l -> match att.adefault with
        | Some e -> Hashtbl.add class_data.def_attributes att.aname  e;add_attributes l
        | None -> match att.atype with
            | Primitive(Int) ->
              let e = { edesc = Val(Int("0"))} in
              Hashtbl.add class_data.def_attributes att.aname  e;add_attributes l
            | Primitive(Boolean) ->
              let e = { edesc = Val(Boolean(false))} in
              Hashtbl.add class_data.def_attributes att.aname  e;add_attributes l
            | Primitive(Float) ->
              let e = { edesc = Val(Float("0"))} in
              Hashtbl.add class_data.def_attributes att.aname  e;add_attributes l
            | Ref (ref) ->
              let e = { edesc = Val(Null)} in
              Hashtbl.add class_data.def_attributes att.aname  e;add_attributes l
            | _ -> raise (CompilingError ("Unrecognized type"))
    in
    	add_attributes classe.cattributes;

			(*print_string "def methods";
    	print_list class_data.def_methods ;

			print_string "list name method";
			print_newline();
			print_list list_name_method;*)

    	Hashtbl.add classes cl.id class_data;

  |_ -> print_endline "this is not a class" ;

	(* STRING OF Value *)
	method string_of_value v = match v with
		| NullValue -> "Null"
		| IntValue i -> string_of_int i
		| FloatValue f -> string_of_float f
		| StringValue s -> s
		| BoolValue b -> string_of_bool b
		| CharValue c -> let string_of_char = String.make 1 in string_of_char c
		| ClassValue r -> "reference to "^ (string_of_int r)
		| NoValue -> "no value"

	(*Print tas*)
	method  string_of_tas =
	(* method that converts Hashtbl of attributes of a tObject into string *)
	let string_of_attributes h =
 		let list_of_attributes = Hashtbl.create 0 in
		let f = (fun key value -> Hashtbl.add list_of_attributes 1 (key^" : "^self#string_of_value value);() ) in
		Hashtbl.iter f h;
		String.concat ";" (Hashtbl.find_all list_of_attributes 1)
	in

	(* method that converts a tObject into string *)
	let	string_of_tobject tobject =
		" nom: "^tobject._name^" type: "^tobject._class^" attributes: "^ (string_of_attributes tobject.attributes)
		in

	let list_of_objects = Hashtbl.create 0 in
	let g = fun key value -> Hashtbl.add list_of_objects 1 "ref: "^string_of_int key^" object: "^string_of_tobject value;() in

	Hashtbl.iter g tas;
	String.concat "\n" (Hashtbl.find_all list_of_objects 1) ;

method string_of_local_scope =
	let list_var_in_local_scope = Hashtbl.create 0 in
	let f = fun key (value:scope_Hashtbl_contenant) -> Hashtbl.add list_var_in_local_scope 1  (" Variable: "^key^" has value: "^ (self#string_of_value (value._value)));  in
	Hashtbl.iter f self#local_scope ;
	String.concat ";" (Hashtbl.find_all list_var_in_local_scope 1);


end
