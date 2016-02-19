open Parser
open Environement
open Execute
open AST

(*
let execute lexbuf verbose =
  try
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast
  with
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
*)

let execute lexbuf verbose =
	let ast = (compilationUnit Lexer.token lexbuf) in
	(
		if (verbose) then (
			 (AST.print_program ast);
		));
		print_endline "----------";
		let ctx = new Environement.environement in
		Execute.gather_toplevel ast.type_list ctx;
		print_endline "---- Execution ----*";
		Execute.execute_main_method ctx;
		print_endline "\n ---- TAS ----";
		if Hashtbl.length ctx#get_tas =0 then print_endline "empty tas" else
		print_string ctx#string_of_tas;
