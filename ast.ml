type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
  And | Or | Madd | Msub | Mmult | Mdiv | Meq | PlusEq  

type uop = Neg | Not

type typ = 
    Int 
  | String 
  | Bool
  | Float 
  | Void  
  | Tuple
  | Matrix
  | Row
  | Column
  | tuple_typ of typ * int
  | matrix_typ of typ * int * int 
  | row_typ of typ * int
  | column_typ of typ * int
  | File 
  

type bind = typ * string

type expr = Int_Lit of int 
  | String_Lit of string
  | Bool_Lit of bool
  | Float_Lit of float
  | Tuple_Lit of expr list
  | Matrix_Lit of expr list list 
  | Row_Lit of expr list
  | Column_Lit of expr list
  | Binop of expr * op * expr 
  | Unop of uop * expr
  | Assign of string * expr 
  | Call of string * expr list
  | Noexpr
            
type stmt = Block of stmt list 
  | Expr of expr 
  | If of expr * stmt * stmt
  | If of expr * stmt * stmt * stmt
  | For of expr * expr * expr * stmt
  | For of expr * expr * stmt
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

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_tuple t = 
let rec string_of_tuple_literal = function 
  () -> ")"
  | [hd] -> (match hd with
      Int_Lit(i)-> string_of_int i
    | _ -> raise( Failure("Illegal expression in tuple primitive") )) ^ string_of_tuple_literal []
  | hd::tl -> (match hd with
      Int_Lit(i) -> string_of_int i ^ ","
    | _ -> raise( Failure("Illegal expression in tuple primitive") )) ^ string_of_tuple_literal tl
in 
"(" ^ string_of_tuple_literal t

let string_of_matrix m r c = 
 let rec string_of_matrix_literal = function
      [] -> "| " ^ string_of_int r ^ ", " ^ string_of_int c ^ "]"
    | [hd] -> (match hd with
                Int_Lit(i) -> string_of_int i
              | Float_Lit(f) -> string_of_float f
              | Tuple_Lit(t) -> string_of_tuple t
              | _ -> raise( Failure("Illegal expression in matrix primitive") )) ^ string_of_matrix_literal []
    | hd::tl -> (match hd with
                    Int_Lit(i) -> string_of_int i ^ ", "
                  | Float_Lit(f) -> string_of_float f ^ ", "
                  | Tuple_Lit(t) -> string_of_tuple t ^ ", "
                  | _ -> raise( Failure("Illegal expression in matrix primitive") )) ^ string_of_matrix_literal tl
  in
  "[|" ^ string_of_matrix_literal m


(* let string_of_row r = 

let string_of_column c =  *)


let rec string_of_expr = function
    Int_Lit(i) -> string_of_int i
  | Bool_Lit(true) -> "True"
  | Bool_Lit(false) -> "False"
  | String_Lit(s) -> s
  | Float_Lit(f) -> string_of_float f 
  | Matrix_Lit(_)-> "matrix literal"
  | Tuple_Lit(t) -> string_of_tuple t 
  | Row_Lit() -> 
  | Column_Lit() -> 
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | If (e, s1, s2, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "elif\n" ^ string_of_stmt s2  
  | If(e, s1, s2, s3) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1 ^ "elif\n" ^ string_of_stmt s2 ^ "else\n" ^ string_of_stmt s3
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | For(e1, e2, s) -> "for (" ^ string_of_expr e1 ^ "in" ^ string_of_expr e2 ^ ")\n" ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Float -> "float"
  | Matrix(t, l1, l2) -> (match t with 
                        Int -> "int" ^ "[" ^ string_of_int l1 ^ "][" ^ string_of_int l2 ^ "]"
                      | Float -> "float" ^ "[" ^ string_of_float l1 ^ "][" ^ string_of_float l2 ^ "]" 
                      | Tuple(x, l) -> (match x with 
                                          Int -> "int" ^ "[" ^ string_of_int l ^ "]"))
  | Tuple(x, l) -> (match x with 
                      Int -> "int" ^ "[" ^ string_of_int l ^ "]" )
  | Row(r, 11) -> (match r with 
                      Int -> "int" ^ "[" ^ string_of_int l1 ^ "]"
                     | Float -> "float" ^ "[" ^ string_of_float l1 ^ "]" 
                     | Tuple(x, l) -> (match x with 
                                        Int -> "int" ^ "[" ^ string_of_int l ^ "]"))
  | Column(c, l1) -> (match c with 
                       Int -> "int" ^ "[" ^ string_of_int l1 ^ "]"
                     | Float -> "float" ^ "[" ^ string_of_float l1 ^ "]" 
                     | Tuple(x, l) -> (match x with 
                                       Int -> "int" ^ "[" ^ string_of_int l ^ "]"))
  | File 
  

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
