%{
  (* Accessing the AST type *)
  open Expr
%}

/**************/
/* The tokens */
/**************/

/* Separators */
%token EOF EOL LPAR RPAR

/* Operators */
%token PLUS MINUS TIMES DIV MOD
%token GT GE LT LE EQ NEQ
%token AND OR NOT

/* Literal values */
%token <int> INT
%token TRUE FALSE

/* Identifiers */
%token <string> IDENT
		     
/* Declaration of variables */
%token LET DEF IN

/********************************/
/* Priorities and associativity */
/********************************/
%nonassoc IN
%left OR
%left AND
%left EQ NEQ
%left GT GE LT LE
%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS NOT

/******************************/
/* Entry points of the parser */
/******************************/
%start expressions
%type < Expr.expression list > expressions


%%
/*************/
/* The rules */
/*************/

expressions:
 | e=expr EOL* EOF               { [e] }
 | e=expr EOL+ rest=expressions   { e::rest }

expr:
  | LET id=IDENT DEF e1=expr IN e2=expr
      { Def(id,e1,e2) }
  | LPAR e=expr RPAR
      { e }
  | NOT e=expr
      { Unop(Unot,e) }
  | MINUS e=expr %prec UMINUS
      { Unop(Uminus,e)}
  | e1=expr o=bop e2=expr
      { Binop(o,e1,e2)}
  | id=IDENT
      { Var id }
  | i=INT
      { Int i }
  | TRUE
      { Bool true }
  | FALSE
      { Bool false }

%inline bop:
  | MINUS     { Bsub }
  | PLUS      { Badd }
  | TIMES     { Bmul }
  | DIV       { Bdiv }
  | MOD       { Bmod }
  | OR        { Bor }
  | AND       { Band }
  | GT        { Bgt }
  | GE        { Bge }
  | LT        { Blt }
  | LE        { Ble }
  | EQ        { Beq }
  | NEQ       { Bneq }
  
%%
