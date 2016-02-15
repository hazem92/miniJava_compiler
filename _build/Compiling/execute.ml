open AST
open Environement





let rec gather_toplevel l ctx = match l with
  |elem::tl -> (match elem.info with
  |Class(x) -> ctx#add_class elem ; gather_toplevel tl ctx);
	| _ ::tl -> gather_toplevel tl ctx
	| [] -> ()

(*me*)
(*let execute_method*)
