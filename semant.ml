(* Semantic checking for the ML compiler *)

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
      (DataType(Int), DataType(Int)) -> lvaluet
    | (DataType(Float), DataType(Float)) -> lvaluet
    | (DataType(Char), DataType(Char)) -> lvaluet
    | (DataType(String), DataType(String)) -> lvaluet
    | (DataType(Bool), DataType(Bool)) -> lvaluet
    | (DataType(Void), DataType(Void)) -> lvaluet
    | (TupleType(Int, l1), TupleType(Int, l2)) -> if l1 == l2 then lvaluet else if l1 == 0 then lvaluet else raise err
    | (MatrixType(DataType(Int), r1, c1), MatrixType(DataType(Int), r2, c2)) -> if r1 == r2 && c1 == c2 then lvaluet else raise err
    | (MatrixType(DataType(Float), r1, c1), MatrixType(DataType(Float), r2, c2)) -> if r1 == r2 && c1 == c2 then lvaluet else raise err
    | (MatrixType(TupleType(Int, d1), r1, c1), MatrixType(TupleType(Int, d2), r2, c2)) -> if d1 == d2 && r1 == r2 && c1 == c2 then lvaluet else raise err
    | (TuplePointerType(Int), TuplePointerType(Int)) -> lvaluet
    | (MatrixPointerType(Int), MatrixPointerType(Int)) -> lvaluet
    | (MatrixPointerType(Float), MatrixPointerType(Float)) -> lvaluet
    | (MatrixTuplePointerType(Int), MatrixTuplePointerType(Int)) -> lvaluet
    | _ -> raise err
  in

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
    locals = []; body = [] } (StringMap.singleton "printb" 
  { typ = Void; fname = "printb"; formals = [(Bool, "x")];
	  locals = []; body = [] })))
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
    let symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m)
	StringMap.empty (globals @ func.formals @ func.locals )
	in

 let type_of_identifier s =
      try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
 in

 let type_of_tuple t =
    match (List.hd t) with
      IntLit _ -> TupleType(Int, List.length t)
    | _ -> raise (Failure ("illegal tuple type")) in

  let rec check_tuple_literal tt l i =
    let length = List.length l in
    match (tt, List.nth l i) with
      (TupleType(Int, _), IntLit _) -> if i == length - 1 then TupleType(Int, length) else check_tuple_literal (TupleType(Int, length)) l (succ i)
    | _ -> raise (Failure ("illegal tuple literal"))
  in

  let access_type = function
      TupleType(p, _) -> DataType(p)
    | _ -> raise (Failure ("illegal access type")) in

  let matrix_acces_type = function
      MatrixType(t, _, _) -> t
    | _ -> raise (Failure ("illegal matrix access") ) in

  let type_of_matrix m r c =
    match (List.hd (List.hd m)) with
        IntLit _ -> MatrixType(DataType(Int), r, c)
      | FloatLit _ -> MatrixType(DataType(Float), r, c)
      | TupleLiteral t -> MatrixType((type_of_tuple) t, r, c)
      | _ -> raise (Failure ("illegal matrix type"))
  in

  let check_pointer_type = function
      TuplePointerType(t) -> TuplePointerType(t)
    | MatrixPointerType(t) -> MatrixPointerType(t)
    | MatrixTuplePointerType(t) -> MatrixTuplePointerType(t)
    | _ -> raise ( Failure ("cannot increment a non-pointer type") )
  in

  let check_tuple_pointer_type = function
      TupleType(p, _) -> TuplePointerType(p)
    | _ -> raise ( Failure ("cannot reference a non-tuple pointer type"))
  in

  let check_matrix_pointer_type = function
      MatrixType(DataType(p), _, _) -> MatrixPointerType(p)
    | _ -> raise ( Failure ("cannot reference a non-matrix pointer type"))
  in

  let check_matrix_tuple_pointer_type = function
      MatrixType(TupleType(p, _), _, _) -> MatrixTuplePointerType(p)
    | _ -> raise ( Failure ("cannot reference a non-matrix-tuple pointer type"))
  in

  let pointer_type = function
    | TuplePointerType(p) -> DataType(p)
    | MatrixPointerType(p) -> DataType(p)
    | MatrixTuplePointerType(p) -> DataType(p)
    | _ -> raise ( Failure ("cannot dereference a non-pointer type") ) in

  (* Return the type of an expression or throw an exception *)
  let rec expr = function
    IntLit _ -> DataType(Int)
  | FloatLit _ -> DataType(Float)
  | CharLit _ -> DataType(Char)
  | StrLit _ -> DataType(String)
  | BoolLit _ -> DataType(Bool)
  | Id s -> type_of_identifier s
  | TupleLiteral t -> check_tuple_literal (type_of_tuple t) t 0
  | MatrixLiteral m -> type_of_matrix m (List.length m) (List.length (List.hd m))
  | TupleAccess(s, e) -> let _ = (match (expr e) with
                                    DataType(Int) -> DataType(Int)
                                  | _ -> raise (Failure ("attempting to access with a non-integer type"))) in
                         access_type (type_of_identifier s)
  | MatrixAccess(s, e1, e2) -> let _ = (match (expr e1) with
                                          DataType(Int) -> DataType(Int)
                                        | _ -> raise (Failure ("attempting to access with a non-integer type")))
                               and _ = (match (expr e2) with
                                          DataType(Int) -> DataType(Int)
                                        | _ -> raise (Failure ("attempting to access with a non-integer type"))) in
                               matrix_acces_type (type_of_identifier s)
  | PointerIncrement(s) -> check_pointer_type (type_of_identifier s)
  | Length(s) -> (match (type_of_identifier s) with
                    TupleType(_, _) -> DataType(Int)
                  | MatrixType(TupleType(_, _), _, _) -> DataType(Int)
                  | _ -> raise(Failure ("cannot get the length of non-matrix-of-tuples or non-tuple datatype")))
  | Rows(s) -> (match (type_of_identifier s) with
                  MatrixType(_, _, _) -> DataType(Int)
                | _ -> raise (Failure ("cannot get the rows of non-matrix datatype")))
  | Columns(s) -> (match (type_of_identifier s) with
                     MatrixType(_, _, _) -> DataType(Int)
                   | _ -> raise (Failure ("cannot get the rows of non-matrix datatype")))
  | Free(s) -> (match (type_of_identifier s) with
                  MatrixType(TupleType(_, _), _, _) -> DataType(Void)
                | _ -> raise (Failure ("cannot free a non-matrix-tuple type")))
  | TupleReference(s) -> check_tuple_pointer_type (type_of_identifier s)
  | Dereference(s) -> pointer_type (type_of_identifier s)
  | MatrixReference(s) -> check_matrix_pointer_type (type_of_identifier s)
  | MatrixTupleReference(s) -> check_matrix_tuple_pointer_type (type_of_identifier s)
  | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
  (match op with
      Add | Sub | Mult | Div when t1 = DataType(Int) && t2 = DataType(Int) -> DataType(Int)
    | Add | Sub | Mult | Div when t1 = DataType(Float) && t2 = DataType(Float) -> DataType(Float)
    | Add | Sub | Mult | Div when t1 = DataType(Int) && t2 = DataType(Float) -> DataType(Float)
    | Madd | Msub | Mmult | Mdiv when t1 MatrixType(Int) && t2 MatrixType(Int) -> MatrixType(Int)
    | Madd | Msub | Mmult | Mdiv when t1 MatrixType(Float) && t2 MatrixType(Float) -> MatrixType(Float)
    | Madd | Msub | Mmult | Mdiv when t1 MatrixType(Int) && t2 MatrixType(Float) -> MatrixType(Float)
    | Madd | Msub | Mmult | Mdiv when t1 MatrixType(TupleType(Int)) && t2 MatrixType(TupleType(Int)) -> MatrixType(TupleType(Int))
    | Madd | Msub | Mmult | Mdiv when t1 MatrixType(TupleType(Int)) && t2 DataType(Int) -> MatrixType(TupleType(Int))
    | Equal | Neq | Meq | when t1 = t2 -> DataType(Bool)
    | PlusEq when t1 = DataType(Int) && t2 = DataType(Int) -> DataType(Int)
    | PlusEq when t1 = DataType(Float) && DataType(Float) -> DataType(Float)
    | PlusEq when t1 = DataType(Int) && t2 = DataType(Float) -> DataType(Float)
    | PlusEq when t1 = DataType(MatrixType) && DataType(MatrixType) -> DataType(MatrixType)
    | PlusEq when t1 = DataType(MatrixType) && DataType(Int) -> DataType(MatrixType)
    | PlusEq when t1 = DataType(MatrixType) && DataType(Float) -> DataType(MatrixType)
    | Less | Leq | Greater | Geq when t1 = DataType(Int) && t2 = DataType(Int) -> DataType(Bool)
    | Less | Leq | Greater | Geq when t1 = DataType(Float) && t2 = DataType(Float) -> DataType(Float)
    | And | Or when t1 = DataType(Bool) && t2 = DataType(Bool) -> DataType(Bool)
    | _ -> raise (Failure ("illegal binary operator " ^
      string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
      string_of_typ t2 ^ " in " ^ string_of_expr e))
  )
  | Unop(op, e) as ex -> let t = expr e in
  (match op with
      Neg when t = DataType(Int) -> DataType(Int)
    | Neg when t = DataType(Float) -> DataType(Float)
    | Not when t = DataType(Bool) -> DataType(Bool)
    | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
      string_of_typ t ^ " in " ^ string_of_expr ex)))
  | Noexpr -> DataType(Void)
  | Assign(e1, e2) as ex -> let lt = (match e1 with
                                        TupleAccess(s, e) -> (match (expr e) with
                                                                DataType(Int) -> (match (type_of_identifier s) with
                                                                                    TupleType(p, _) -> (match p with
                                                                                                          Int -> DataType(Int)
                                                                                                       )
                                                                                   | _ -> raise ( Failure ("cannot access a non-tuple type") )
                                                                                 )
                                                                | _ -> raise ( Failure ("expression is not of type int") )
                                                             )
                                      | MatrixAccess(s, _, _) -> (match (type_of_identifier s) with
                                                                      MatrixType(t, _, _) -> (match t with
                                                                                                    DataType(Int) -> DataType(Int)
                                                                                                  | DataType(Float) -> DataType(Float)
                                                                                                  | TupleType(p, l) -> TupleType(p, l)
                                                                                                  | _ -> raise ( Failure ("illegal matrix of matrices") )
                                                                                                )
                                                                      | _ -> raise ( Failure ("cannot access a primitive") )
                                                                   )
                                      | _ -> expr e1)
                            and rt = (match e2 with
                                        TupleAccess(s, e) -> (match (expr e) with
                                                                DataType(Int) -> (match (type_of_identifier s) with
                                                                                    TupleType(p, _) -> (match p with
                                                                                                          Int -> DataType(Int)
                                                                                                       )
                                                                                   | _ -> raise ( Failure ("cannot access a non-tuple type") )
                                                                                 )
                                                                | _ -> raise ( Failure ("expression is not of datatype int") )
                                                             )
                                      | MatrixAccess(s, _, _) -> (match (type_of_identifier s) with
                                                                      MatrixType(t, _, _) -> (match t with
                                                                                                  DataType(Int) -> DataType(Int)
                                                                                                | DataType(Float) -> DataType(Float)
                                                                                                | TupleType(p, l) -> TupleType(p, l)
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
   fd.datatype
  in

  let check_bool_expr e =
    match (expr e) with
      DataType(Bool) -> ()
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
  | Return e -> let t = expr e in if t = func.datatype then () else
  raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
   string_of_typ func.datatype ^ " in " ^ string_of_expr e))

  | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
  | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
  ignore (expr e3); stmt st
  | MFor(e1, e2, s) -> ignore (expr e1); ignore (expr e2); stmt s
  | While(p, s) -> check_bool_expr p; stmt s
  in

  stmt (Block func.body)

  in
  List.iter check_function functions
