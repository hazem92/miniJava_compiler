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

let execute lexbuf verbose execOnly=
  
	let ast = (compilationUnit Lexer.token lexbuf) in
  if not(execOnly) then (
    print_endline "-----------typing--------";
    let env = new TypeEnv.global_env in
    TypeExec.typeOuter_program ast env;
    TypeExec.typeInner_program ast env;
    ASTType.type_program env ast;
  );
	(
		if (verbose) then (
			 (AST.print_program ast);
		)
  );

  print_endline "----------";

  let env = new Environement.environement in
  Execute.gather_toplevel ast.type_list env;
  print_endline "---- Execution ----*";
  Execute.execute_main_method env;
  print_string "\n ---- TAS ---- \n";
  if Hashtbl.length env#get_tas =0 then print_endline "empty tas" else
  print_string env#string_of_tas;
