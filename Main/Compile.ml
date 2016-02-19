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
		Execute.execute_main_method ctx;
