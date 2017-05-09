open Ast

module StringMap = Map.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

  let check_assign lvaluet rvaluet err =
    match (lvaluet, rvaluet) with
      (Int, Int) -> lvaluet
    | (Float, Float) -> lvaluet
    | (String, String) -> lvaluet
    | (Char, Char) -> lvaluet
    | (Bool, Bool) -> lvaluet
    | (Void, Void) -> lvaluet
    | (TupleTyp(Int, l1), TupleTyp(Int, l2)) -> if l1 == l2 then lvaluet else if l1 == 0 then lvaluet else raise err
    | (RowTyp(Int, l1), RowTyp(Int, l2)) -> if l1 == l2 then lvaluet else if l1 == 0 then lvaluet else raise err 
    | (RowTyp(Float, l1), RowTyp(Float, l2)) -> if l1 == l2 then lvaluet else if l1 == 0 then lvaluet else raise err
    | (RowTyp(TupleTyp(Int, d1), l1), RowTyp(TupleTyp(Int, d2), l2)) -> if d1 == d2 && l1 == l2 then lvaluet else if l1 == 0 then lvaluet else raise err
    | (MatrixTyp(Int, r1, c1), MatrixTyp(Int, r2, c2)) -> if r1 == r2 && c1 == c2 then lvaluet else raise err
    | (MatrixTyp(Float, r1, c1), MatrixTyp(Float, r2, c2)) -> if r1 == r2 && c1 == c2 then lvaluet else raise err
    | (MatrixTyp(TupleTyp(Int, d1), r1, c1), MatrixTyp(TupleTyp(Int, d2), r2, c2)) -> if d1 == d2 && r1 == r2 && c1 == c2 then lvaluet else raise err
    (* | (TuplePointerType(Int), TuplePointerType(Int)) -> lvaluet
    | (MatrixPointerType(Int), MatrixPointerType(Int)) -> lvaluet
    | (MatrixPointerType(Float), MatrixPointerType(Float)) -> lvaluet
    | (MatrixTuplePointerType(Int), MatrixTuplePointerType(Int)) -> lvaluet *)
    | (RowPointer(Int), RowPointer(Int)) -> lvaluet
    | (MatrixPointer(Int), MatrixPointer(Int)) -> lvaluet
    | _ -> raise err
  in
(*
  let _ print_c1 print_c2  = match (value1, value2) with
  println(c1)-> value1
    | println(c2)) -> value2

  in*)

  (*List.iter(fun j -> print_string (string_of_typ (fst j)); print_string " ") fd.formals;*)

  (**** Checking Global Variables ****)

  List.iter (check_not_void (fun n -> "illegal void global " ^ n)) globals;

  report_duplicate (fun n -> "duplicate global " ^ n) (List.map snd globals);

    (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  if List.mem "prints" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  if List.mem "printb" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  if List.mem "printf" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
  (List.map (fun fd -> fd.fname) functions);

  let built_in_decls = StringMap.add "print"
  { typ = Void; fname = "print"; formals = [(Int, "x")]; 
    locals = []; body = [] } (StringMap.add "printf"
  { typ = Void; fname = "printf"; formals = [(Float, "x")];
    locals = []; body = [] } (StringMap.add "prints"
  { typ = Void; fname = "prints"; formals = [(String, "x")];
    locals = []; body = [] } (StringMap.add "printb" 
  { typ = Void; fname = "printb"; formals = [(Bool, "x")];
    locals = []; body = [] } (StringMap.add "open"
  { typ = String; fname = "open"; formals = [(String, "x"); (String,"y")];
    locals = []; body = [] } (StringMap.add "read"
  { typ = String; fname = "read"; formals = [(String,"w"); (Int, "x"); (Int, "y"); (String, "z")];
    locals = []; body = [] } (StringMap.add "write"
  { typ = Int; fname = "write"; formals = [(String, "w"); (Int, "x"); (Int, "y"); (String, "z")];
    locals = []; body = [] } (StringMap.add "close"
  { typ = Void; fname = "close"; formals = [(String, "x")];
    locals = []; body = [] } (StringMap.add "fget"
  { typ = String; fname = "fget"; formals = [(String, "x"); (Int, "y"); (String, "z")];
    locals = []; body = [] } (StringMap.add "atoi"
  { typ = Int ; fname = "atoi"; formals = [(String, "x")];
    locals = []; body = [] } (StringMap.add "itos"
  { typ = Void ; fname = "itos"; formals = [(String, "x"); (String,"y"); (Int, "z")];
    locals = []; body = [] } (StringMap.add "splitstr"
  { typ = String ; fname = "splitstr"; formals = [(String, "x"); (String,"y")];
    locals = []; body = [] } (StringMap.singleton "len"
  { typ = Int; fname = "len"; formals = [(String, "x")];
    locals = []; body = [] } ))))))))))))

in

let function_decls = 
	List.fold_left (fun m fd -> StringMap.add fd.fname fd m) built_in_decls functions
in

let function_decl s = try StringMap.find s function_decls 
	with Not_found -> raise (Failure ("Unrecognized function " ^ s))
in

let _ = function_decl "main" in

(* A function that is used to check each function *)
let check_function func =


	List.iter(check_not_void (fun n -> 
		"Illegal void formal " ^ n ^ " in " ^ func.fname)) func.formals;

	report_duplicate (fun n ->
		"Duplicate formal " ^ n ^ " in " ^ func.fname)(List.map snd func.formals);

	List.iter (check_not_void (fun n ->
		"Illegal void local " ^ n ^ " in " ^ func.fname)) func.locals;

	report_duplicate (fun n ->
		"Duplicate local " ^ n ^ " in " ^ func.fname)(List.map snd func.locals);

(* Check variables *)
  let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m) (* name type map *)
    StringMap.empty (globals @ func.formals @ func.locals )
  in

  let symbols = ref symbols in


 let type_of_tuple t =
    match (List.hd t) with
      IntLit _ -> TupleTyp(Int, List.length t)
    | _ -> raise (Failure ("illegal tuple type")) in

let find_rowtyp name m =
	let m = StringMap.find m !symbols in
	let typ = match m with
		MatrixTyp(Int, _, _) -> Int
	  | MatrixTyp(Float, _, _) -> Float
	  | MatrixTyp(TupleTyp(Int, len), _, _) -> TupleTyp(Int, len)
	  | _ -> raise (Failure ("illegal matrix type")) in
	let cols = match m with
		MatrixTyp(_, _, c) -> c
	  | _ -> raise (Failure ("illegal matrix type")) in
	symbols := StringMap.add name (RowTyp(typ, cols)) !symbols in

 let type_of_identifier s =
      try StringMap.find s !symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
   in

  let rec check_tuple_literal tt l i =
    let length = List.length l in
    match (tt, List.nth l i) with
      (TupleTyp(Int, _), IntLit _) -> if i == length - 1 then TupleTyp(Int, length) else check_tuple_literal (TupleTyp(Int, length)) l (succ i)
    | _ -> raise (Failure ("illegal tuple literal"))
  in

  let tuple_access_type = function
      TupleTyp(p, _) -> p
    | _ -> raise (Failure ("illegal tuple access")) in

  let row_access_type = function
      RowTyp(r, _) -> r
    | _ -> raise (Failure ("illegal row access")) in

  let matrix_access_type = function
      MatrixTyp(t, _, _) -> t
    | _ -> raise (Failure ("illegal matrix access") ) in

  let mrow_access_type = function
	    MatrixTyp(t, _, c) -> RowTyp(t, c)
	  | _ -> raise (Failure ("illegal matrix access") ) in


  let type_of_row r l =
	match (List.hd r) with
        IntLit _ -> RowTyp(Int, l)
      | FloatLit _ -> RowTyp(Float, l)
      | TupleLit t -> RowTyp((type_of_tuple) t, l)
      | _ -> raise (Failure ("illegal row type"))
  in

  let type_of_matrix m r c =
    match (List.hd (List.hd m)) with
        IntLit _ -> MatrixTyp(Int, r, c)
      | FloatLit _ -> MatrixTyp(Float, r, c)
      | TupleLit t -> MatrixTyp((type_of_tuple) t, r, c)
      | _ -> raise (Failure ("illegal matrix type"))
  in

 let check_pointer_type = function
    (*  TuplePointerType(t) -> TuplePointerType(t)
    | MatrixPointerType(t) -> MatrixPointerType(t)
    | MatrixTuplePointerType(t) -> MatrixTuplePointerType(t)*)
      RowPointer(t) -> RowPointer(t)
    | MatrixPointer(t) -> MatrixPointer(t)
    | _ -> raise ( Failure ("cannot increment a non-pointer type") )
  in

    let matrix_type s = match (List.hd s) with
    | IntLit _ -> RowTyp(Int, List.length s)
    | FloatLit _ -> RowTyp(Float, List.length s)
    | BoolLit _ -> RowTyp(Bool, List.length s)
    | _ -> raise ( Failure ("Cannot instantiate a matrix of that type")) in

    let rec check_all_matrix_literal m ty idx =
      let length = List.length m in
        match (ty, List.nth m idx) with
      (RowTyp(Int, _), IntLit _) -> if idx == length - 1 then RowTyp(Int, length) else check_all_matrix_literal m (RowTyp(Int, length)) (succ idx)
    | (RowTyp(Float, _), FloatLit _) -> if idx == length - 1 then RowTyp(Float, length) else check_all_matrix_literal m (RowTyp(Float, length)) (succ idx)
    | (RowTyp(Bool, _), BoolLit _) -> if idx == length - 1 then RowTyp(Bool, length) else check_all_matrix_literal m (RowTyp(Bool, length)) (succ idx)
    | _ -> raise (Failure ("illegal matrix literal"))
  in


  let check_row_pointer_type = function
    RowTyp(p, _) -> RowPointer(p)
    | _ -> raise ( Failure ("cannot reference non-row pointer type"))
  in

(*
  let check_tuple_pointer_type = function
      TupleType(p, _) -> TuplePointerType(p)
    | _ -> raise ( Failure ("cannot reference a non-tuple pointer type"))
  in *)

  let check_matrix_pointer_type = function
      MatrixTyp(p, _, _) -> MatrixPointer(p)
    | _ -> raise ( Failure ("cannot reference a non-matrix pointer type"))
  in

(*
  let check_matrix_tuple_pointer_type = function
      MatrixType(TupleType(p, _), _, _) -> MatrixTuplePointerType(p)
    | _ -> raise ( Failure ("cannot reference a non-matrix-tuple pointer type"))
  in
*)

  let pointer_type = function
   (* | TuplePointerType(p) -> DataType(p)
    | MatrixPointerType(p) -> DataType(p)
    | MatrixTuplePointerType(p) -> DataType(p) *)
    | RowPointer(t) -> t
    | MatrixPointer(t) -> t
    | _ -> raise ( Failure ("cannot dereference a non-pointer type") ) in

  (* Return the type of an expression or throw an exception *)
  let rec expr = function
    IntLit _ -> Int
  | FloatLit _ -> Float
  | StringLit _ -> String 
  | CharLit _ -> Char
  | BoolLit _ -> Bool
  | Id s -> type_of_identifier s
  | RowLit r -> type_of_row r (List.length r)
  | TupleLit t -> check_tuple_literal (type_of_tuple t) t 0
  | MatrixLit m -> type_of_matrix m (List.length m) (List.length (List.hd m))
  | RowAccess(s, e) -> let _ = (match (expr e) with
                                    Int -> Int
                                  | _ -> raise (Failure ("attempting to access with non-integer type"))) in
                            row_access_type (type_of_identifier s)
  | TupleAccess(s, e) -> let _ = (match (expr e) with
                                    Int -> Int
                                  | _ -> raise (Failure ("attempting to access with a non-integer type"))) in
                         tuple_access_type (type_of_identifier s)
  | MatrixAccess(s, e1, e2) -> let _ = (match (expr e1) with
                                          Int -> Int
                                        | _ -> raise (Failure ("attempting to access with a non-integer type")))
                               and _ = (match (expr e2) with
                                          Int -> Int
                                        | _ -> raise (Failure ("attempting to access with a non-integer type"))) in
                               matrix_access_type (type_of_identifier s)
  | MRowAccess(s, e) -> let _ = (match (expr e) with
									Int -> Int
								  | _ -> raise (Failure ("attempting to access with non-integer type"))) in
							mrow_access_type (type_of_identifier s)
  | RowReference(s) -> check_row_pointer_type( type_of_identifier s )
  | PointerIncrement(s) -> check_pointer_type (type_of_identifier s)
  (*| RowLit(s) -> (match (type_of_identifier s) with
                  MatrixTyp(_, _, _) -> Int
                | _ -> raise (Failure ("cannot get the rows of non-matrix datatype"))) *)
(*   | Free(s) -> (match (type_of_identifier s) with
                  MatrixTyp(TupleTyp(_, _), _, _) -> Void
                | _ -> raise (Failure ("cannot free a non-matrix-tuple type")))
  | TupleReference(s) -> check_tuple_pointer_type (type_of_identifier s) *)
  | Dereference(s) -> pointer_type (type_of_identifier s)
  | MatrixReference(s) -> check_matrix_pointer_type (type_of_identifier s)
  (*| MatrixTupleReference(s) -> check_matrix_tuple_pointer_type (type_of_identifier s) *)
  | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
  (match op with
    Add -> (match t1,t2 with Int,Int -> Int
      | Float,Float -> Float
      | TupleTyp(Int,l1),TupleTyp(Int,l2) when l1=l2 -> TupleTyp(Int,l1)
      | TupleTyp(Int,l1), Int -> TupleTyp(Int, l1)
      | Int, TupleTyp(Int,l1) -> TupleTyp(Int, l1)
      | MatrixTyp(Int,r1,c1),MatrixTyp(Int,r2,c2) when r1=r2 && c1=c2 -> MatrixTyp(Int,r1,c1)
      | MatrixTyp(Int,r1,c1), Int -> MatrixTyp(Int,r1,c1)
      | Int, MatrixTyp(Int,r1,c1) -> MatrixTyp(Int,r1,c1)
      | MatrixTyp(Float,r1,c1),MatrixTyp(Float,r2,c2) when r1=r2 && c1=c2 -> MatrixTyp(Float,r1,c1)
      | MatrixTyp(Float,r1,c1), Float -> MatrixTyp(Float,r1,c1)
      | Float, MatrixTyp(Float,r1,c1) -> MatrixTyp(Float,r1,c1)
      | _,_ -> raise (Failure("illegal addition operator")))
    | Sub -> (match t1,t2 with Int,Int -> Int
      | Float,Float -> Float
      | TupleTyp(Int,l1),TupleTyp(Int,l2) when l1=l2 -> TupleTyp(Int,l1)
      | TupleTyp(Int,l1), Int -> TupleTyp(Int, l1)
      | Int, TupleTyp(Int,l1) -> TupleTyp(Int, l1)
      | MatrixTyp(Int,r1,c1),MatrixTyp(Int,r2,c2) when r1=r2 && c1=c2 -> MatrixTyp(Int,r1,c1)
      | MatrixTyp(Int,r1,c1), Int -> MatrixTyp(Int,r1,c1)
      | Int, MatrixTyp(Int,r1,c1) -> MatrixTyp(Int,r1,c1)
      | MatrixTyp(Float,r1,c1),MatrixTyp(Float,r2,c2) when r1=r2 && c1=c2 -> MatrixTyp(Float,r1,c1)
      | MatrixTyp(Float,r1,c1), Float -> MatrixTyp(Float,r1,c1)
      | Float, MatrixTyp(Float,r1,c1) -> MatrixTyp(Float,r1,c1) 
      | _,_ -> raise (Failure("illegal subtraction operator")))
    | Mult -> (match t1,t2 with Int,Int -> Int
      | Float,Float -> Float
      | TupleTyp(Int, l1), Int -> TupleTyp(Int, l1)
      | Int, TupleTyp(Int, l1) -> TupleTyp(Int, l1)
      | Int, MatrixTyp(Int,r1,c1) -> MatrixTyp(Int,r1,c1)
      | MatrixTyp(Int,r1,c1), Int -> MatrixTyp(Int,r1,c1)
      | Float, MatrixTyp(Float,r1,c1) -> MatrixTyp(Float,r1,c1)
      | MatrixTyp(Float,r1,c1), Float -> MatrixTyp(Float,r1,c1)
      | _,_ -> raise (Failure("illegal multiplication operator"))) 
    | Div -> (match t1,t2 with Int,Int -> Int
      | Float,Float -> Float
      | TupleTyp(Int, l1), Int -> TupleTyp(Int, l1)
      | Int, TupleTyp(Int, l1) -> TupleTyp(Int, l1)
      | Int, MatrixTyp(Int,r1,c1) -> MatrixTyp(Int,r1,c1)
      | MatrixTyp(Int,r1,c1), Int -> MatrixTyp(Int,r1,c1)
      | Float, MatrixTyp(Float,r1,c1) -> MatrixTyp(Float,r1,c1)
      | MatrixTyp(Float,r1,c1), Float -> MatrixTyp(Float,r1,c1)
      | _,_ -> raise (Failure("illegal division operator"))) 
    | Equal | Neq | Meq when t1 = t2 -> Bool
    | PlusEq when t1 = Int && t2 = Int -> Int
    | PlusEq when t1 = Int && t2 = Float -> Float
    (* | PlusEq when t1 = MatrixTyp && MatrixTyp -> MatrixTyp
    | PlusEq when t1 = MatrixTyp && Int -> MatrixTyp
    | PlusEq when t1 = MatrixTyp && Float -> MatrixTyp
    *) | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
    | Less | Leq | Greater | Geq when t1 = Float && t2 = Float -> Float
    | And | Or when t1 = Bool && t2 = Bool -> Bool
    | _ -> raise (Failure ("illegal binary operator " ^
      string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
      string_of_typ t2 ^ " in " ^ string_of_expr e))
  )
  | Unop(op, e) as ex -> let t = expr e in
  (match op with
      Neg when t = Int -> Int
    | Neg when t = Float -> Float
    | Not when t = Bool -> Bool
    | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
      string_of_typ t ^ " in " ^ string_of_expr ex)))
  | Init(var, lit) -> let a = type_of_identifier var and b= expr lit in
    (match b with Int -> a
    | _ -> raise (Failure("illegal "^ string_of_typ b ^", expected int")))
  | Noexpr -> Void
  | Assign(e1, e2) as ex -> let lt = (match e1 with
                                      | RowAccess(s, _) -> (match (type_of_identifier s) with
                                                                      RowTyp(t, _) -> (match t with
                                                                                                    Int -> Int
                                                                                                  | Float -> Float
                                                                                                  | TupleTyp(p, l) -> TupleTyp(p, l) 
                                                                                                  | _ -> raise ( Failure ("illegal row") )
                                                                                                )
                                                                      | _ -> raise ( Failure ("cannot access a primitive") )
                                                                   )
                                      | TupleAccess(s, e) -> (match (expr e) with
                                                                Int -> (match (type_of_identifier s) with
                                                                                    TupleTyp(p, _) -> (match p with
                                                                                                          Int -> Int
                                                                                                       )
                                                                                   | _ -> raise ( Failure ("cannot access a non-tuple type") )
                                                                                 )
                                                                | _ -> raise ( Failure ("expression is not of type int") )
                                                             )
                                      | MatrixAccess(s, _, _) -> (match (type_of_identifier s) with
                                                                      MatrixTyp(t, _, _) -> (match t with
                                                                                                    Int -> Int
                                                                                                  | Float -> Float
                                                                                                  | TupleTyp(p, l) -> TupleTyp(p, l)
                                                                                                  | _ -> raise ( Failure ("illegal matrix of matrices") )
                                                                                                )
                                                                      | _ -> raise ( Failure ("cannot access a primitive") )
                                                                   )
                                      | _ -> expr e1)
                            and rt = (match e2 with
                                      | RowAccess(s, _) -> (match (type_of_identifier s) with
                                                                      RowTyp(t, _) -> (match t with
                                                                                                    Int -> Int
                                                                                                  | Float -> Float
                                                                                                  | TupleTyp(p, l) -> TupleTyp(p, l)
                                                                                                  | _ -> raise ( Failure ("illegal row") )
                                                                                                )
                                                                      | _ -> raise ( Failure ("cannot access a primitive") )
                                                                   )
                                      | TupleAccess(s, e) -> (match (expr e) with
                                                                Int -> (match (type_of_identifier s) with
                                                                                    TupleTyp(p, _) -> (match p with
                                                                                                          Int -> Int
                                                                                                       )
                                                                                   | _ -> raise ( Failure ("cannot access a non-tuple type") )
                                                                                 )
                                                                | _ -> raise ( Failure ("expression is not of datatype int") )
                                                             )
                                      | MatrixAccess(s, _, _) -> (match (type_of_identifier s) with
                                                                      MatrixTyp(t, _, _) -> (match t with

                                                                                                  Int -> Int
                                                                                                | Float -> Float
                                                                                                | TupleTyp(p, l) -> TupleTyp(p, l)
                                                                                                | _ -> raise ( Failure ("illegal matrix of matrices") )
                                                                                              )
                                                                      | _ -> raise ( Failure ("cannot access a primitive") )
                                                                   )
                                      | _ -> expr e2) in
  check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
    " = " ^ string_of_typ rt ^ " in " ^
  string_of_expr ex))
  | Call(fname, actuals) as call -> let fd = function_decl fname in
  if List.length actuals != List.length fd.formals then
   raise (Failure ("expecting " ^ string_of_int
     (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
  else
   List.iter2 (fun (ft, _) e -> let et = expr e in
    ignore (check_assign ft et
      (Failure ("illegal actual argument found " ^ string_of_typ et ^
        " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
   fd.formals actuals;
   fd.typ
  in 

  let check_bool_expr e =
    match (expr e) with
      Bool -> ()
    | _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
  in

  (* Verify a statement or throw an exception *)
  let rec stmt = function
  Block sl -> let rec check_block = function
  [Return _ as s] -> stmt s
  | Return _ :: _ -> raise (Failure "nothing may follow a return")
  | Block sl :: ss -> check_block (sl @ ss)
  | s :: ss -> stmt s ; check_block ss
  | [] -> ()
  in check_block sl
  | Expr e -> ignore (expr e)
  | Return e -> let t = expr e in if t = func.typ then () else
  raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
   string_of_typ func.typ ^ " in " ^ string_of_expr e))

  | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
  | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
  ignore (expr e3); stmt st
  | MFor(s1, s2, s) -> find_rowtyp s1 s2; ignore(s1); ignore(s2); stmt s
  | While(p, s) -> check_bool_expr p; stmt s
  in

  stmt (Block func.body)

  in
  List.iter check_function functions
  