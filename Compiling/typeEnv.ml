open AST
open Exceptions



class local_env =
  object (self)
    
    val attributes = Hashtbl.create 0; 
    val methods = Hashtbl.create 0;
    val consts = Hashtbl.create 0;


    method get_att = attributes
    method add_att (a:astattribute) =
      if (Hashtbl.mem attributes a.aname) 
      then
        (raise (TypingError (" attribut de meme nom deja defini ")))
      else
        (Hashtbl.add attributes a.aname a)

    method get_meth = methods
    method add_meth (m:astmethod) = 
      if (Hashtbl.mem methods m.mname) 
      then
        (raise (TypingError (" methode de meme nom deja definie : cette version de compilateur ne supporte pas la surcharge ")))
      else
        (Hashtbl.add methods m.mname m)

      
  end





class global_env =
  object (self)
    val mutable package = [""]
    val classes = Hashtbl.create 0
    val local_class_env = Hashtbl.create 0

    method get_loc_env nom_class = Hashtbl.find local_class_env nom_class

    method get_package = package
    method setpack str = package <- str 

    method get_classes = classes
    method add_class t = 
      if  ((Hashtbl.mem classes t.id))
      then (raise (TypingError (" class de meme nom \" "^t.id^" \" deja definie ")); print_newline())
      else (
        Hashtbl.add classes t.id t;
        Hashtbl.add local_class_env t.id (new local_env)
      )

    method check_parent c =
      if (c <> "Object")
        then
          (if not (Hashtbl.mem classes c)
            then (raise (TypingError (" class parent \" "^c^" \" non definie "))))


    method get_class (nom_class:string) =
      (* print_endline ("Get class "^nom_class); *) 
      if not (Hashtbl.mem classes nom_class)
      then (raise (TypingError (" class non definie "^nom_class)));
      Hashtbl.find classes nom_class
    
    method get_attribute_defined_in_class (nom_att:string) (nom_class:string) = 
      if not (Hashtbl.mem classes nom_class)
      then (raise (TypingError (" class non definie "^nom_class)));
      if not (Hashtbl.mem (Hashtbl.find local_class_env nom_class)#get_att nom_att)
      then (raise(TypingError (" attribut "^nom_att^" non defini"))); 
      Hashtbl.find (Hashtbl.find local_class_env nom_class)#get_att nom_att

    method get_method_defined_in_class (nom_meth:string) (nom_class:string) = 
      if not (Hashtbl.mem classes nom_class)
      then (raise (TypingError (" class non definie "^nom_class)));
      if not (Hashtbl.mem (Hashtbl.find local_class_env nom_class)#get_meth nom_meth)
      then (raise(TypingError (" method "^nom_meth^" non defini "))); 
      Hashtbl.find (Hashtbl.find local_class_env nom_class)#get_meth nom_meth

    method get_method (nom_meth:string) (nom_class:string)  =
      (* print_endline ("Get method: "^nom_meth^" in class "^nom_class); *)
      if (nom_class = "Object")
      then (self#get_method_defined_in_class nom_meth nom_class) else
      if not (Hashtbl.mem classes nom_class)
      then (raise (TypingError (" class "^nom_class^" non definie "))) else 
      if not (Hashtbl.mem (Hashtbl.find local_class_env nom_class)#get_meth nom_meth)
        then (match (Hashtbl.find classes nom_class).info with
            |Class c -> self#get_method (nom_meth) (c.cparent.tid);) else
        (Hashtbl.find (Hashtbl.find local_class_env nom_class)#get_meth nom_meth)
     

    method get_attribute (nom_att:string) (nom_class:string)  =
      (* print_endline ("Get attribute: "^nom_att^" in class "^nom_class); *)
      if (nom_class = "Object")
      then (self#get_attribute_defined_in_class nom_att nom_class) else
      if not (Hashtbl.mem classes nom_class)
      then (raise (TypingError (" class "^nom_class^" non definie "))) else 
      if not (Hashtbl.mem (Hashtbl.find local_class_env nom_class)#get_att nom_att)
        then (match (Hashtbl.find classes nom_class).info with
            |Class c -> self#get_attribute (nom_att) (c.cparent.tid);) else
        (Hashtbl.find (Hashtbl.find local_class_env nom_class)#get_att nom_att)
       
      
      
      

 end
  




