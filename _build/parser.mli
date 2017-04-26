type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | BAR
  | COLON
  | LSQBRACE
  | RSQBRACE
  | LPERCENT
  | RPERCENT
  | LMPERCENT
  | RMPERCENT
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | MPLUS
  | MMINUS
  | MTIMES
  | MDIVIDE
  | PLUSEQ
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | MEQ
  | TRUE
  | FALSE
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | VOID
  | MATRIX
  | ROW
  | FLOAT
  | FILE
  | TUPLE
  | INT_LIT of (int)
  | STRING_LIT of (string)
  | ID of (string)
  | FLOAT_LIT of (float)
  | DEF
  | IN
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
