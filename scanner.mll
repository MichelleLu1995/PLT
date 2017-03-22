(* Ocamllex scanner for MicroC *)

{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQBRACE }
| ']'      { RSQBRACE }
| ';'      { SEMI }
| ':'      { COLON }
| ','      { COMMA }
| '+'      { PLUS }
| "++"     { MPLUS }
| '-'      { MINUS }
| "--"     { MMINUS }
| '*'      { TIMES }
| "**"     { MTIMES }
| '/'      { DIVIDE }
| "//"     { MDIVIDE }
| '='      { ASSIGN }
| '^'      { EXPONENT }
| "=="     { EQ }
| "!="     { NEQ }
| "=?"     { MEQ }
| "+="     { PLUSEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "elif"   { ELIF }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "string" { STRING }
| "float"  { FLOAT }
| "void"   { VOID }
| "mx"     { MATRIX }
| "row"    { ROW }
| "column" { COLUMN }
| "file"   { FILE }
| "tuple"  { TUPLE }
| "print"  { PRINT }
| "main"   { MAIN }
| "def"    { DEF }
| "in"     { IN }

| "True"|"False" as lxm { BOOL_LIT(bool_of_string lxm) }
| ['0'-'9']+'.'['0'-'9']+ as lxm { FLOAT_LIT(float_of_string lxm) }
| ['0'-'9']+ as lxm { INT_LIT(int_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '"'(['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ' ']* as lxm)'"' { STRING_LIT(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }


(*
{
let main () =
	let lexbuf = Lexing.from_channel stdin in
	try
		while true do
			ignore (token lexbuf)
		done
	with _ -> print_string "invalid_token\n"
let _ = Printexc.print main ()

}*)
