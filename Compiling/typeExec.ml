open TypeEnv
open Type
open AST


let verify_parent t global_environement=
  (match t.info with
   | Class c -> global_environement#check_parent c.cparent.tid ; 
   | Inter -> ())

let typer_type t global_environement=
  (match t.info with
   | Class c ->  global_environement#add_class t ; 
   | Inter -> ())
    
  (*construire l'environnement global : on ajoute d'abord les classes pour garantir
    la possibilité de définir les classes dans n'importe qu'elle ordre (permettre d'heriter une 
    classe définie après dans le fichier )*)
let typeOuter_program p global_environement=
  (match p.package with
  | None -> global_environement#setpack ["none"]
  | Some pack -> global_environement#setpack pack);
  List.iter (fun t -> typer_type t global_environement) p.type_list; (*ajouter d'abord les classes*)
  List.iter (fun t -> verify_parent t global_environement) p.type_list (*verifier les parents*)

  



(*--------------*)
let type_method local_environement m =
  local_environement#add_meth m

let type_attribute local_environement a  =
  local_environement#add_att a

 
let typer_class id c global_environement=
	let local_environement = (global_environement#get_loc_env id) in
  List.iter (type_attribute local_environement) c.cattributes ;
  (* List.iter (print_const ("  ")) c.cconsts; -> à faire traitement des constructeurs  *)
  List.iter (type_method local_environement) c.cmethods 

let typerInner_class t global_environement=
  (match t.info with
   | Class c -> typer_class t.id c global_environement;
   | Inter -> ())
 
 (*construire l'environnement local de chaque classes *)
 let typeInner_program p global_environement = 
  List.iter (fun t -> typerInner_class t global_environement;  print_newline()) p.type_list;
  
  (*------------------*)

  
