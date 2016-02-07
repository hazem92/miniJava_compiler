exception Error

type token = 
  | TRUE
  | TIMES
  | RPAR
  | PLUS
  | OR
  | NOT
  | NEQ
  | MOD
  | MINUS
  | LT
  | LPAR
  | LET
  | LE
  | INT of (int)
  | IN
  | IDENT of (string)
  | GT
  | GE
  | FALSE
  | EQ
  | EOL
  | EOF
  | DIV
  | DEF
  | AND


val expressions: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ( Expr.expression list )