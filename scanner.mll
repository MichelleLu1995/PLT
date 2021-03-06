(* Ocamllex scanner for MicroC *)

{ open Parser 

  let un_esc s =
	Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

let digits = ['0'-'9']
let alphabet = ['a'-'z' 'A'-'Z']
let dig_or_alpha = digits| alphabet| '_'
let id = alphabet dig_or_alpha*

let esc_char = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii_sym = ([' '-'!' '#'-'[' ']'-'~'])
let char = '"' ( ascii_sym | digits ) '"'
let string = '"' ( (ascii_sym | esc_char)* as s ) '"'

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQBRACE }
| ']'      { RSQBRACE }
| "(%"	   { LPERCENT }
| "%)"	   { RPERCENT }
| "{%"     { LMPERCENT }
| "%}"	   { RMPERCENT }
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| '|'	   { BAR }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| "@="     { ATASSIGN }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "void"   { VOID }
| "length" { LENGTH }
| "width"  { WIDTH }
| "type"   { TYPE }
| "tuple"  { TUPLE }
| "def"    { DEF }
| "in"     { IN }
| "True"   { TRUE }
| "False"  { FALSE }
| "$"	     { DOLLAR }
| "#"	     { OCTOTHORP }
| "new"    { NEW }
| "String" { STRING }
| "File"   { STRING }
| "~"	   { SQUIGLY }
| "NULL"   { NULL}
| ['0'-'9']+'.'['0'-'9']+ as lxm { FLOAT_LIT(float_of_string lxm) }
| '.'	   { DOT }
| ['0'-'9']+ as lxm { INT_LIT(int_of_string lxm) }
| id as lxm { ID(lxm) }
| string       				{ STRING_LIT(un_esc s) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
