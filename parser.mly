%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA BAR COLON LSQBRACE RSQBRACE LPERCENT RPERCENT LMPERCENT RMPERCENT
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT MPLUS MMINUS MTIMES MDIVIDE PLUSEQ
%token EQ NEQ LT LEQ GT GEQ AND OR MEQ
%token TRUE FALSE
%token RETURN IF ELSE FOR WHILE INT BOOL VOID MATRIX FLOAT TUPLE STRING CHAR
%token OCTOTHORP DOLLAR SQUIGLY

%token <int> INT_LIT
%token <string> STRING_LIT 
%token <string> ID
%token <char> CHAR_LIT
%token <float> FLOAT_LIT

%token DEF
%token IN
%token EOF

%nonassoc NOELSE
%nonassoc NOLSQBRACE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ MEQ
%left PLUS MINUS MPLUS MMINUS PLUSEQ
%left TIMES DIVIDE MTIMES MDIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   DEF typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $2;
	 fname = $3;
	 formals = $5;
	 locals = List.rev $8;
	 body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

matrix_typ:
  primitive LSQBRACE INT_LIT RSQBRACE LSQBRACE INT_LIT RSQBRACE { MatrixTyp($1, $3, $6) }
	
row_typ:
  primitive LSQBRACE INT_LIT RSQBRACE { RowTyp($1, $3) }

tuple_typ:
  primitive LPERCENT INT_LIT RPERCENT { TupleTyp($1, $3) }

row_pointer_typ:
  primitive LSQBRACE RSQBRACE { RowPointer($1) }

matrix_pointer_typ:
  primitive LSQBRACE RSQBRACE LSQBRACE RSQBRACE { MatrixPointer($1) }

typ:
  primitive { $1 }
  | MATRIX { Matrix }
  | matrix_typ { $1 }
  | row_typ { $1 }

primitive:
  	INT { Int }
  | BOOL { Bool }
  | VOID { Void }
  | CHAR {Char}
  | FLOAT { Float }
  | tuple_typ { $1 }
  | row_pointer_typ { $1 }
  | matrix_pointer_typ { $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | FOR LPAREN ID IN ID RPAREN stmt { MFor($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    literals          { $1 }
  | ID				 { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr MPLUS  expr { Binop($1, Madd,  $3) }
  | expr MMINUS expr { Binop($1, Msub,  $3) }
  | expr MTIMES expr { Binop($1, Mmult, $3) }
  | expr MDIVIDE expr{ Binop($1, Mdiv,  $3) }
  | expr MEQ    expr { Binop($1, Meq,   $3) }
  | expr PLUSEQ expr { Binop($1, PlusEq,$3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | expr ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | DOLLAR ID { RowReference($2) }
  | DOLLAR DOLLAR ID { MatrixReference($3) }
  | OCTOTHORP ID { Dereference($2) }
  | SQUIGLY SQUIGLY ID { PointerIncrement($3) }
  | ID LSQBRACE expr RSQBRACE %prec NOLSQBRACE { RowAccess($1, $3) }

  | ID LPERCENT expr RPERCENT { TupleAccess($1, $3) }
  | ID LSQBRACE expr RSQBRACE LSQBRACE expr RSQBRACE { MatrixAccess($1, $3, $6) }
  | ID LSQBRACE expr RSQBRACE LSQBRACE COLON RSQBRACE { MRowAccess($1, $3) }
  | ID LSQBRACE COLON RSQBRACE LSQBRACE expr RSQBRACE { MColumnAccess($1, $6) }

primitives:
	INT_LIT      { IntLit($1) }
  | FLOAT_LIT  { FloatLit($1) }
  | STRING_LIT { StringLit($1) }
  | CHAR_LIT   { CharLit($1) }
  | TRUE       { BoolLit(true) }
  | FALSE      { BoolLit(false) }

literals:
	primitives { $1 }
  |	tuple_literal	{ $1 } 
  |	LSQBRACE primitive_rowlit RSQBRACE { RowLit(List.rev $2) }
  | LSQBRACE tuple_rowlit RSQBRACE { RowLit(List.rev $2) }
  | LMPERCENT primitive_matrixlit RMPERCENT { MatrixLit(List.rev $2) }
  | LMPERCENT tuple_matrixlit RMPERCENT { MatrixLit(List.rev $2) }

array_literal:
	primitives { [$1] } 
  | array_literal COMMA primitives { $3 :: $1 }

tuple_literal:
	LPERCENT array_literal RPERCENT { TupleLit(List.rev $2) }

primitive_rowlit:
	primitives { [$1] }
  | primitive_rowlit COMMA primitives { $3 :: $1 }

tuple_rowlit:
	tuple_literal { [$1] } 
  | tuple_rowlit COMMA tuple_literal { $3 :: $1 }

tuple_matrixlit:
	tuple_rowlit { [$1] }
  | tuple_matrixlit BAR tuple_rowlit { $3 :: $1 }

primitive_matrixlit:
	primitive_rowlit { [$1] }
  | primitive_matrixlit BAR primitive_rowlit { $3 :: $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
