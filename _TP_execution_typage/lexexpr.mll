{
  open Parseexpr
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let space = [' ' '\t']

rule nexttoken = parse
  | space+        { nexttoken lexbuf }
  | '\n'          { EOL }
  | eof           { EOF }
  | "&&"          { AND }
  | "||"          { OR }
  | "!"           { NOT }
  | "("           { LPAR }
  | ")"           { RPAR }
  | ">"           { GT }
  | ">="          { GE }
  | "<"           { LT }
  | "<="          { LE }
  | "=="          { EQ }
  | "!="          { NEQ }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "let"         { LET } 
  | "="           { DEF }
  | "in"          { IN }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "/"           { DIV }
  | "*"           { TIMES }
  | "%"           { MOD }
  | digit+ as nb  { INT (int_of_string nb) }
  | ident as str  { IDENT str }
