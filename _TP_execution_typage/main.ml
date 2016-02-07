open Parseexpr
open Lexexpr
open Expr

let treat_expression exp =
  print_string (string_of_expr exp);
  print_string " : ";
  try
      print_string (string_of_type (typing [] exp));
      print_string " = ";
      print_string (string_of_value (eval [] exp));
      print_newline()
  with Unbound_variable s ->
    print_endline ("Variable "^s^" inconnue!")
     | Wrong_types_bop(op, x, y) ->
    print_string ("L'operateur "^(string_of_op_b op));
    print_string (" attend deux arguments de type "^(string_type_of_op_b op));
    print_string (" et il reçoit "^(string_of_type x));
    print_endline (" et "^(string_of_type y))
     | Wrong_types_uop(op, x) ->
    print_string ("L'operateur "^(string_of_op_u op));
    print_string (" attend un argument de type "^(string_type_of_op_u op));
    print_endline (" et il reçoit "^(string_of_type x))

let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    let exp_list = expressions nexttoken lexbuf in
    close_in (input_file);
    List.iter treat_expression exp_list;
    exit 0
  with Sys_error s ->
    print_endline ("Can't find file '" ^ file ^ "'")

let _ = Arg.parse [] compile ""
