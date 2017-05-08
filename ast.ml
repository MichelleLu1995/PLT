type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
  And | Or | Madd | Msub | Mmult | Mdiv | Meq | PlusEq  

type uop = Neg | Not

type typ = 
	Int
  | Float
  | String 
  | Char
  | Bool
  | Void  
  | Tuple
  | Matrix
  | Row
  | Column
  | MatrixTyp of typ * int * int 
  | RowTyp of typ * int
  | ColumnTyp of typ * int
  | TupleTyp of typ * int
  | RowPointer of typ
  | MatrixPointer of typ

type bind = typ * string

type expr = IntLit of int 
  | Id of string
  | StringLit of string
  | CharLit of char
  | BoolLit of bool
  | FloatLit of float
  | TupleLit of expr list
  | MatrixLit of expr list list 
  | RowLit of expr list
  (*| ColumnLit of expr list*)
  | Binop of expr * op * expr 
  | Unop of uop * expr
  | Assign of expr * expr 
  | Call of string * expr list
  | RowAccess of string * expr
  | TupleAccess of string * expr
  | MatrixAccess of string * expr * expr
  | MRowAccess of string * expr
  | MColumnAccess of string * expr
  | Noexpr
  | MatrixReference of string
  | PointerIncrement of string
  | Dereference of string
  | RowReference of string
            
type stmt = Block of stmt list 
  | Expr of expr 
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | MFor of string * string * stmt
  | While of expr * stmt
  | Return of expr
            
type func_decl = { 
  typ : typ;
  fname : string; 
  formals : bind list; 
  locals : bind list; 
  body : stmt list;
}   
      
type program = bind list * func_decl list 
        
(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Madd -> "++"
  | Msub -> "--"
  | Mmult -> "**"
  | Mdiv -> "//"
  | Meq -> "=?"
  | PlusEq -> "+="

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_tuple t = 
  let rec string_of_tuple_literal = function 
    [] -> ")"
  | [hd] -> (match hd with
      IntLit(i)-> string_of_int i
    | _ -> raise( Failure("Illegal expression in tuple primitive") )) ^ string_of_tuple_literal []
  | hd :: tl -> (match hd with
	  IntLit(i) -> string_of_int i ^ ", "
    | _ -> raise( Failure("Illegal expression in tuple primitive") )) ^ string_of_tuple_literal tl
in 
"(" ^ string_of_tuple_literal t

let string_of_row r =
  let rec string_of_row_literal = function
	[] -> "]"
  | [hd] -> (match hd with
	  IntLit(i) -> string_of_int i
	| FloatLit(f) -> string_of_float f
	| TupleLit(t) -> string_of_tuple t
	| _ -> raise( Failure("Illegal expression in row primitive") )) ^ string_of_row_literal []
  | hd :: tl -> (match hd with
	  IntLit(i) -> string_of_int i ^ ", "
	| FloatLit(f) -> string_of_float f ^ ", "
	| TupleLit(t) -> string_of_tuple t ^ ", "
	| _ -> raise( Failure("Illegal expression in row primitive") )) ^ string_of_row_literal tl
in
"[" ^ string_of_row_literal r
(*
let string_of_column c =
  let rec string_of_column_literal = function
	[] -> "]"
  | [hd] -> (match hd with
	  IntLit(i) -> string_of_int i
	| FloatLit(f) -> string_of_float f
	| TupleLit(t) -> string_of_tuple t
	| _ -> raise( Failure("Illegal expression in column primitive") )) ^ string_of_column_literal []
  | hd :: tl -> (match hd with
	  IntLit(i) -> string_of_int i ^ "| "
	| FloatLit(f) -> string_of_float f ^ "| "
	| TupleLit(t) -> string_of_tuple t ^ "| "
	| _ -> raise( Failure("Illegal expression in column primitive") )) ^ string_of_column_literal tl
in
"[" ^ string_of_column_literal c*)

let string_of_matrix m = 
 let rec string_of_matrix_literal = function
      [] -> "}"
    | [hd] -> (match hd with
                RowLit(r) -> string_of_row r
              | _ -> raise( Failure("Illegal expression in matrix primitive") )) ^ string_of_matrix_literal []
    | hd::tl -> (match hd with
                    RowLit(r) -> string_of_row r ^ ", "
                  | _ -> raise( Failure("Illegal expression in matrix primitive") )) ^ string_of_matrix_literal tl
  in
  "{" ^ string_of_matrix_literal m

let rec string_of_expr = function
    IntLit(i) -> string_of_int i
  | BoolLit(b) -> string_of_bool b
  | StringLit(s) -> s
  | CharLit(s) -> Char.escaped s
  | Id(i) -> i
  | FloatLit(f) -> string_of_float f 
  | MatrixLit(_)-> "matrix literal"
  | TupleLit(t) -> string_of_tuple t 
  | RowLit(r) -> string_of_row r 
  (*| ColumnLit(c) -> string_of_column c*)
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | RowAccess(r, e) -> r ^ "[" ^ string_of_expr e ^ "]"
  | TupleAccess(t, e) -> t ^ "(%" ^ string_of_expr e ^ "%)"
  | MatrixAccess(m, e1, e2) -> m ^ "[" ^ string_of_expr e1 ^ "][" ^ string_of_expr e2 ^ "]"
  | MRowAccess(r, e) -> r ^ "[" ^ string_of_expr e ^ "][:]"
  | MColumnAccess(c, e) -> c ^ "[:][" ^ string_of_expr e ^ "]"
  | Noexpr -> ""
  | RowReference(s) -> "$" ^ s
  | MatrixReference(s) -> "$$" ^ s
  | Dereference(s) -> "#" ^ s
  | PointerIncrement(s) -> "~~" ^ s

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | MFor(s1, s2, s) -> "for (" ^ s1 ^ " in " ^ s2 ^ ")\n" ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Float -> "float"
  | String -> "string"
  | Char -> "char"
  | MatrixTyp(t, l1, l2) -> (match t with 
                        Int -> "int" ^ "[" ^ string_of_int l1 ^ "][" ^ string_of_int l2 ^ "]"
                      | Float -> "float" ^ "[" ^ string_of_int l1 ^ "][" ^ string_of_int l2 ^ "]" 
                      | TupleTyp(x, l) -> (match x with 
                                          Int -> "int" ^ "(" ^ string_of_int l ^ ")"
										| _ -> raise( Failure("Illegal expression in tuple primitive") ))
					  | _ -> raise( Failure("Illegal expression in matrix primitive")))
  | TupleTyp(x, l) -> (match x with 
                      Int -> "int" ^ "(" ^ string_of_int l ^ ")" 
					 | _ -> raise( Failure("Illegal expression in tuple primitive")))
  | RowTyp(r, l1) -> (match r with 
                      Int -> "int" ^ "[" ^ string_of_int l1 ^ "]"
                     | Float -> "float" ^ "[" ^ string_of_int l1 ^ "]" 
                     | TupleTyp(x, l) -> (match x with 
                                          Int -> "int" ^ "(" ^ string_of_int l ^ ")"
										| _ -> raise( Failure("Illegal expression in tuple primitive") ))
					 | _ -> raise( Failure("Illegal expression in row primitive")))
  | RowPointer(t) -> string_of_typ t ^ "[]"
  | MatrixPointer(t) -> string_of_typ t ^ "[][]"
  (* | RowPointer(Int) -> "int[]" *)
  (*| ColumnTyp(c, l1) -> (match c with 
                       Int -> "int" ^ "[" ^ string_of_int l1 ^ "]"
                     | Float -> "float" ^ "[" ^ string_of_int l1 ^ "]" 
                     | TupleTyp(x, l) -> (match x with 
                                          Int -> "int" ^ "(" ^ string_of_int l ^ ")"
										| _ -> raise( Failure("Illegal expression in tuple primitive") ))
					 | _ -> raise( Failure("Illegal expression in column primitive")))*)
  (*| File*) 
  | _ -> raise( Failure("Illegal expression in string_of_typ"))
  

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
