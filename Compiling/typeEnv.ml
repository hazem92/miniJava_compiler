open AST
open Exceptions

(*il s'agit de deux classe qui représentent l'une l'environnement global : l'ensemble des noms de 
classes et les paramètres correspendants et l'autre représente l'environnement local pour chaque classe*)


(*cette classe est l'environnement local de chaque classe, on verifie qu'il n y a pas deux attributs de meme nom 
et ainsi pour les methodes (cette classe doit etre modifier si on souhaite prendre en compte la surcharge ) *)
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


(*dans l'environnement on a une methode qui permet de récupérer la lite contenant les classes
ainsi qu'une autre qui permet d'ajouter les classes en parcourants l'ast*)
(*avant d'ajouter une classe on verifie qu'il n y a pas une autre classe qui est déja definie*)


class global_env =
  object (self)
    val mutable package = [""]
    val classes = Hashtbl.create 0
    val local_class_env = Hashtbl.create 0
    (*renvoie l'environnement local d'une classe dont le nom est passé en params*)
    method get_loc_env nom_class = Hashtbl.find local_class_env nom_class
    (*renvoie le package de l'environnement : cette methode est pour d'eventuelle évolution si on decide de travailler sur plusieurs packages*)
    method get_package = package
    
    method setpack str = package <- str 
    (*renvoie la liste des classes*)
    method get_classes = classes
    (*ajouter une classe, verifier d'abord qu'il n y a pas d'autre classe de meme nom*)
    method add_class t = 
      if  ((Hashtbl.mem classes t.id))
      then (raise (TypingError (" class de meme nom \" "^t.id^" \" deja definie ")); print_newline())
      else (
        Hashtbl.add classes t.id t;
        Hashtbl.add local_class_env t.id (new local_env)
      )


    method check_parent c =
      if (c <> "Object") (*cette condition pour empecher de verifier la classe mère de la classe Object puisqu'elle n'a pas de class parent ...*)
        then
          (if not (Hashtbl.mem classes c)
            then (raise (TypingError (" class parent \" "^c^" \" non definie "))))

    (*renvoie les données d'une classes*)
    method get_class (nom_class:string) =
      (* print_endline ("Get class "^nom_class); *) 
      if not (Hashtbl.mem classes nom_class)
      then (raise (TypingError (" class non definie "^nom_class)));
      Hashtbl.find classes nom_class
    
    (*chercher un attribue d'une classe, lever une erreur si l'attribue n'existe pas*)
    method get_attribute_defined_in_class (nom_att:string) (nom_class:string) = 
      if not (Hashtbl.mem classes nom_class)
      then (raise (TypingError (" class non definie "^nom_class)));
      if not (Hashtbl.mem (Hashtbl.find local_class_env nom_class)#get_att nom_att)
      then (raise(TypingError (" attribut "^nom_att^" non defini"))); 
      Hashtbl.find (Hashtbl.find local_class_env nom_class)#get_att nom_att

      (*chercher une methode dans une classe, lever une erreur le cas echéant*)
    method get_method_defined_in_class (nom_meth:string) (nom_class:string) = 
      if not (Hashtbl.mem classes nom_class)
      then (raise (TypingError (" class non definie "^nom_class)));
      if not (Hashtbl.mem (Hashtbl.find local_class_env nom_class)#get_meth nom_meth)
      then (raise(TypingError (" method "^nom_meth^" non defini "))); 
      Hashtbl.find (Hashtbl.find local_class_env nom_class)#get_meth nom_meth

      (*chercher une methode dans une classe et dans l'ensemble de ses parents, renvoie la 
      premiere methode trouvée si non on leve une erreur*)
    method get_method (nom_meth:string) (nom_class:string)  =
      if (nom_class = "Object")(* Class Object doit etre definie déja // le meme 
      traitement pour les classes String, Integer ... -> une solution sera de les coder et les faire 
      passer au programme (voir test : bigTest.java)*)
      then (self#get_method_defined_in_class nom_meth nom_class) else
      if not (Hashtbl.mem classes nom_class)
      then (raise (TypingError (" class "^nom_class^" non definie "))) else 
      if not (Hashtbl.mem (Hashtbl.find local_class_env nom_class)#get_meth nom_meth)
        then (match (Hashtbl.find classes nom_class).info with
            |Class c -> self#get_method (nom_meth) (c.cparent.tid);) else
        (Hashtbl.find (Hashtbl.find local_class_env nom_class)#get_meth nom_meth)
     
        (*chercher une attribut dans une classe et dans l'ensemble de ses parents*)
        (*à ajouter : il faut verifier les duplications des attributs *)
    method get_attribute (nom_att:string) (nom_class:string)  =
      if (nom_class = "Object")
      then (self#get_attribute_defined_in_class nom_att nom_class) else
      if not (Hashtbl.mem classes nom_class)
      then (raise (TypingError (" class "^nom_class^" non definie "))) else 
      if not (Hashtbl.mem (Hashtbl.find local_class_env nom_class)#get_att nom_att)
        then (match (Hashtbl.find classes nom_class).info with
            |Class c -> self#get_attribute (nom_att) (c.cparent.tid);) else
        (Hashtbl.find (Hashtbl.find local_class_env nom_class)#get_att nom_att)
       
      
      
      

 end
  




