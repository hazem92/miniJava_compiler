open Type
open AST
open Exceptions

(* Definitions of used Data structures in environement
   Class will have a list of string of its defined methods, the implementation of these methods
   will be contained in the global_methodes attribute
*)

type class_data = {mutable def_attributes:(string,string) Hashtbl.t; mutable def_methods:string list ; mutable parent:string}
type method_data = astmethod

(*Definition of the class environement *)
class environement =
  object (self)

  val global_methodes = Hashtbl.create 0 ;
  val classes = (let c = Hashtbl.create 0 in

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

(*avoid having interface instance +++todo+++ *)
(*  Compiling Class.. adding class to the enviroment
    Check if Parent is defined and no other class is defined having the same name
    Building the table of methods
 *)

  method add_class (cl : asttype ) =     match cl.info with
  |Class classe ->
    if ( not (Hashtbl.mem classes classe.cparent.tid)) then
    raise (CompilingError ("The parent Class of " ^ cl.id ^ " is not known"));
    if (Hashtbl.mem classes cl.id) then
    raise (CompilingError ("Class " ^ cl.id ^ " is already defined"));
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

    let rec method_list_name = function
    | [] -> []
    | x :: l ->(classe.cparent.tid^"_"^x.mname)::(method_list_name l) in
    let list_name_method = method_list_name method_list in

    let verify_methods  = function
      | x -> (if not (List.mem x list_name_method) then (class_data.def_methods <- x::class_data.def_methods ;))
    in

    List.map verify_methods parent_class.def_methods;

    let rec print_list = function
    [] -> print_string cl.id ; print_endline ""
    | e::l -> print_string e ; print_endline "" ; print_list l
    in

	(*print_string "def methods";
    print_list class_data.def_methods ;

	print_string "list name method";
	print_newline();
	print_list list_name_method;*)

    Hashtbl.add classes cl.id class_data;


  |_ -> print_endline "this is not a class" ;


  end
