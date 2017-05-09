(* Code generation: translate takes a semantically checked AST and produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Exceptions

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "JSTEM"
  and i32_t  = L.i32_type  context
  and float_t   = L.double_type context
  and i8_t   = L.i8_type   context
  and pointer_t = L.pointer_type
  and array_t   = L.array_type
  and i1_t   = L.i1_type   context
  and void_t = L.void_type context in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Void -> void_t
    | A.Float -> float_t
    | A.String -> pointer_t i8_t
    | A.String_T -> L.pointer_type (pointer_t i8_t)
    | A.Char -> i8_t
    | A.TupleTyp(typ, size) -> (match typ with
                                  A.Int    -> array_t i32_t size
                                | _ -> raise (UnsupportedTupleType))
    | A.MatrixTyp(typ, size1, size2) -> (match typ with
                                      A.Int    -> array_t (array_t i32_t size2) size1
                                    | A.Float  -> array_t (array_t float_t size2) size1
                                    | A.TupleTyp(typ1,size3) -> (match typ1 with
                                              A.Int    -> array_t (array_t (array_t i32_t size3) size2) size1
                                            | _ -> raise (UnsupportedTupleType))
                                    | _ -> raise (UnsupportedMatrixType))
    | A.RowTyp(typ, size) -> (match typ with
                                      A.Int    -> array_t i32_t size
                                    | A.Float  -> array_t float_t size
                                    | A.TupleTyp(typ1,size1) -> (match typ1 with
                                              A.Int    -> array_t (array_t i32_t size1) size 
                                            | _ -> raise (UnsupportedTupleType))
                                    | _ -> raise (UnsupportedRowType))
    | A.RowPointer(t) -> (match t with
                              A.Int -> pointer_t i32_t
                            | A.Float -> pointer_t float_t
                            | _ -> raise (IllegalPointerType))
    | A.MatrixPointer(t) -> (match t with
                              A.Int -> pointer_t i32_t
                            | A.Float -> pointer_t float_t
                            | _ -> raise (IllegalPointerType))
    | _ -> raise (Failure("TypeNotFound"))
    (* | _ -> raise (Failure ("illegal type " ^ A.string_of_typ )) *)
  in
 

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Printing *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let prints_t = L.var_arg_function_type (pointer_t i8_t) [|L.pointer_type i8_t|] in
  let prints_func = L.declare_function "puts" prints_t the_module in

  (* file inegration *)
  let open_ty = L.function_type (pointer_t i8_t) [| (pointer_t i8_t) ; L.pointer_type i8_t |] in
  let open_func = L.declare_function "fopen" open_ty the_module in 
  let close_ty = L.function_type i32_t [| (pointer_t i8_t) |] in
  let close_func = L.declare_function "fclose" close_ty the_module in
  let read_ty = L.function_type i32_t [| (pointer_t i8_t); i32_t; i32_t; (pointer_t i8_t) |] in 
  let read_func = L.declare_function "fread" read_ty the_module in
  let get_ty = L.function_type (pointer_t i8_t) [| (pointer_t i8_t); i32_t; (pointer_t i8_t) |] in 
  let get_func = L.declare_function "fgets" get_ty the_module in
  let fwrite_ty = L.function_type i32_t [| (pointer_t i8_t); i32_t; i32_t; (pointer_t i8_t) |] in
  let fwrite_func = L.declare_function "fwrite" fwrite_ty the_module in

  (* String functions *)
  let strlen_ty = L.function_type i32_t [| (pointer_t i8_t) |] in
  let strlen_func = L.declare_function "strlen" strlen_ty the_module in
  let cast_str_int_ty = L.function_type i32_t [| (pointer_t i8_t) |] in
  let cast_str_int_func = L.declare_function "atoi" cast_str_int_ty the_module in
  let sprintf_ty = L.function_type (pointer_t i8_t) [| (pointer_t i8_t); L.pointer_type i8_t; i32_t|] in
  let sprintf_func = L.declare_function "sprintf" sprintf_ty the_module in
  let calloc_ty = L.function_type (pointer_t i8_t) [|i32_t; i32_t|] in
  let calloc_func = L.declare_function "calloc" calloc_ty the_module in
  let strtok_ty = L.function_type (pointer_t i8_t) [| (pointer_t i8_t) ; L.pointer_type i8_t |] in
  let strtok_fun = L.declare_function "strtok" strtok_ty the_module in
  
  (* Data casting *)

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    let float_format_str = L.build_global_stringptr "%f\n" "fmt" builder in
    let char_format_str = L.build_global_stringptr "%c\n" "fmt" builder in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)

  let types = ref StringMap.empty in

    let local_vars = 
      let add_formal m (t, n) p = L.set_value_name n p;
  let local = L.build_alloca (ltype_of_typ t) n builder in
  ignore (L.build_store p local builder); 
  StringMap.add n local m in

      let add_local m (t, n) = (* print_endline(A.string_of_typ t); *)

      types := StringMap.add n t !types;

  let local_var = (* L.build_alloca (ltype_of_typ t) n builder *)
   (match t with
       A.MatrixTyp(A.TupleTyp(_, l), r, c) -> if r * c * l > 1000
                                                  then L.build_malloc (ltype_of_typ t) n builder
                                                else
                                                  L.build_alloca (ltype_of_typ t) n builder
     | _ -> L.build_alloca (ltype_of_typ t) n builder)
  in StringMap.add n local_var m in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

	let local_vars = ref local_vars in

	(* ADD INDEX VARIABLE TO LOCALS FOR MATRIX FOR LOOP *)
	local_vars := StringMap.add "_i" (L.build_alloca (ltype_of_typ A.Int) "_i" builder) !local_vars;
	
	let check_function = List.fold_left (fun m (t, n) -> StringMap.add n t m)
		StringMap.empty (globals @ fdecl.A.formals @ fdecl.A.locals) in
	
	let check_function = ref check_function in


	(* ADD ROW TO LOCALS FOR MATRIX FOR LOOP *)
	let allocate_row name len m =

    let typ = match (StringMap.find m !types) with
      A.MatrixTyp(A.Int, _, _) -> A.Int
      | A.MatrixTyp(A.Float, _, _) -> A.Float
      | A.MatrixTyp(A.TupleTyp(A.Int, len), _, _) -> A.TupleTyp(A.Int, len)
      | _ -> raise (Failure ("illegal matrix type")) in

		check_function := StringMap.add name (A.RowTyp(typ, len)) !check_function;
		check_function := StringMap.add "_i" (A.Int) !check_function;
		local_vars := StringMap.add name (L.build_alloca (ltype_of_typ (A.RowTyp(typ, len))) name builder) !local_vars in

    (*let print_vars key value = print_endline(key) in
    StringMap.iter print_vars local_vars; *)

    (* Return the value for a variable or formal argument *)
    let lookup n = 
					try StringMap.find n !local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    let type_of_identifier s =
      let symbols = !check_function in
      StringMap.find s symbols
    in

    let build_row_argument s builder =
      L.build_in_bounds_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t 0 |] s builder
    in

    let build_matrix_argument s builder =
      L.build_in_bounds_gep (lookup s) [| L.const_int i32_t 0; L.const_int i32_t 0; L.const_int i32_t 0 |] s builder
    in

    let build_pointer_dereference s builder isAssign = 
      if isAssign
        then L.build_load (lookup s) s builder
      else
        L.build_load (L.build_load (lookup s) s builder) s builder
    in

    let build_pointer_increment s builder isAssign = 
      if isAssign
        then L.build_load (L.build_in_bounds_gep (lookup s) [| L.const_int i32_t 1 |] s builder) s builder
      else
        L.build_in_bounds_gep (L.build_load (L.build_in_bounds_gep (lookup s) [| L.const_int i32_t 0 |] s builder) s builder) [| L.const_int i32_t 1 |] s builder
    in

    let get_tuple_type tuple =
      match (List.hd tuple) with
        A.IntLit _ -> ltype_of_typ (A.Int)
      | _ -> raise (UnsupportedTupleType) in

    let get_matrix_type matrix =
      match (List.hd matrix) with
        A.IntLit _ -> ltype_of_typ (A.Int)
      | A.FloatLit _ -> ltype_of_typ (A.Float)
      | A.TupleLit _ -> ltype_of_typ (A.Tuple)
      | _ -> raise (UnsupportedMatrixType) in

    let get_row_type row =
      match (List.hd row) with
        A.IntLit _ -> ltype_of_typ (A.Int)
      | A.FloatLit _ -> ltype_of_typ (A.Float)
      | A.TupleLit _ -> ltype_of_typ (A.Tuple)
      | _ -> raise (UnsupportedRowType) in

  let build_row_access s i1 i2 builder isAssign =
    if isAssign
    then L.build_gep (lookup s) [| i1; i2 |] s builder
    else
    L.build_load (L.build_gep (lookup s) [| i1; i2 |] s builder) s builder
  in

  let build_tuple_access s i1 i2 builder isAssign =
    if isAssign
    then L.build_gep (lookup s) [| i1; i2 |] s builder
    else
    L.build_load (L.build_gep (lookup s) [| i1; i2 |] s builder) s builder
  in

  let build_matrix_access s i1 i2 i3 builder isAssign =
    if isAssign
    then L.build_gep (lookup s) [| i1; i2; i3|] s builder
    else
    L.build_load (L.build_gep (lookup s) [| i1; i2; i3 |] s builder) s builder
  in

    (* New code/supporting functions for getting type *)
    (* A function that is used to check each function *)
	let build_mrow_access s i1 i2 builder isAssign =
	  if isAssign
		then L.build_gep (lookup s) [| i1; i2 |] s builder
	  else
		L.build_load (L.build_gep (lookup s) [| i1; i2 |] s builder) s builder
	in

 let type_of_tuple1 t =
    match (List.hd t) with
      A.IntLit _ -> A.TupleTyp(A.Int, List.length t)
    | _ -> raise (Failure ("illegal tuple type")) in

  let rec check_tuple_literal1 tt l i =
    let length = List.length l in
    match (tt, List.nth l i) with
      (A.TupleTyp(A.Int, _), A.IntLit _) -> if i == length - 1 then A.TupleTyp(A.Int, length) else check_tuple_literal1 (A.TupleTyp(A.Int, length)) l (succ i)
    | _ -> raise (Failure ("illegal tuple literal"))
  in

  let type_of_row1 r l =
  match (List.hd r) with
        A.IntLit _ -> A.RowTyp(A.Int, l)
      | A.FloatLit _ -> A.RowTyp(A.Float, l)
      | A.TupleLit t -> A.RowTyp((type_of_tuple1) t, l)
      | _ -> raise (Failure ("illegal row type"))
  in

  let type_of_matrix1 m r c =
    match (List.hd (List.hd m)) with
        A.IntLit _ -> A.MatrixTyp(A.Int, r, c)
      | A.FloatLit _ -> A.MatrixTyp(A.Float, r, c)
      | A.TupleLit t -> A.MatrixTyp((type_of_tuple1) t, r, c)
      | _ -> raise (Failure ("illegal matrix type"))
  in

  let expr1 = function
    A.IntLit _ -> A.Int
  | A.FloatLit _ -> A.Float
  | A.StringLit _ -> A.String
  | A.BoolLit _ -> A.Bool
  | A.Id s -> type_of_identifier s
  | A.RowLit r -> type_of_row1 r (List.length r)
  | A.TupleLit t -> check_tuple_literal1 (type_of_tuple1 t) t 0
  | A.MatrixLit m -> type_of_matrix1 m (List.length m) (List.length (List.hd m))
  | A.Binop(e1, op, e2) -> let t1 = expr1 e1 and t2 = expr1 e2 in
  (match op with
        A.Add -> (match t1,t2 with A.Int,A.Int -> A.Int
      | A.Float,A.Float | A.Int,A.Float | A.Float,A.Int -> A.Float
      | A.TupleTyp(A.Int,l1),A.TupleTyp(A.Int,l2) when l1=l2 -> A.TupleTyp(A.Int,l1)
      | A.MatrixTyp(A.Int,r1,c1),A.MatrixTyp(A.Int,r2,c2) when r1=r2 && c1=c2 -> A.MatrixTyp(A.Int,r1,c1)
      | A.MatrixTyp(A.Float,r1,c1),A.MatrixTyp(A.Float,r2,c2) when r1=r2 && c1=c2 -> A.MatrixTyp(A.Float,r1,c1)
      | _,_ -> raise (Failure("illegal type")))
    | A.Sub -> (match t1,t2 with A.Int,A.Int -> A.Int
      | A.Float,A.Float | A.Int,A.Float | A.Float,A.Int -> A.Float
      | A.TupleTyp(A.Int,l1),A.TupleTyp(A.Int,l2) when l1=l2 -> A.TupleTyp(A.Int,l1)
      | A.MatrixTyp(A.Int,r1,c1),A.MatrixTyp(A.Int,r2,c2) when r1=r2 && c1=c2 -> A.MatrixTyp(A.Int,r1,c1)
      | A.MatrixTyp(A.Float,r1,c1),A.MatrixTyp(A.Float,r2,c2) when r1=r2 && c1=c2 -> A.MatrixTyp(A.Float,r1,c1)
      | _,_ -> raise (Failure("illegal type")))
    | A.Mult -> (match t1,t2 with A.Int,A.Int -> A.Int
      | A.Float,A.Float | A.Int,A.Float | A.Float,A.Int -> A.Float
      | _,_ -> raise (Failure("illegal type")))
    | A.Div -> (match t1,t2 with A.Int,A.Int -> A.Int
      | A.Float,A.Float | A.Int,A.Float | A.Float,A.Int -> A.Float
      | _,_ -> raise (Failure("illegal type")))
    | _ -> raise (Failure ("illegal binop")))
  | A.Unop(op, e) -> let t = expr1 e in
  (match op with
      A.Neg when t = A.Int -> A.Int
    | A.Neg when t = A.Float -> A.Float
    | A.Not when t = A.Bool -> A.Bool
    | _ -> raise (Failure ("illegal unop")))
  | A.Call(_,_) -> A.Int
  | _ -> raise (Failure("illegal expression"))
  in

  let build_mrow_access s i1 i2 builder isAssign =
    if isAssign
    then L.build_gep (lookup s) [| i1; i2 |] s builder
    else
    L.build_load (L.build_gep (lookup s) [| i1; i2 |] s builder) s builder
  in

  let build_matrix_access s i1 i2 i3 builder isAssign =
    if isAssign
    then L.build_gep (lookup s) [| i1; i2; i3|] s builder
    else
    L.build_load (L.build_gep (lookup s) [| i1; i2; i3 |] s builder) s builder
  in

  let build_mrow_access s i1 i2 builder isAssign =
    if isAssign
    then L.build_gep (lookup s) [| i1; i2 |] s builder
    else
    L.build_load (L.build_gep (lookup s) [| i1; i2 |] s builder) s builder
  in

	let get_length s =
		L.array_length (L.type_of (L.build_load (L.build_gep (lookup s) [| L.const_int i32_t 0 |] s builder) s builder)) in

    (* Construct code for an expression; return its value *)
    let rec expr builder = function
        A.IntLit i -> L.const_int i32_t i
      | A.FloatLit f -> L.const_float float_t f
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.StringLit s -> L.build_global_stringptr s "string" builder
      | A.CharLit c -> L.const_int i8_t (Char.code c)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup s) s builder
      | A.TupleLit t -> L.const_array (get_tuple_type t) (Array.of_list (List.map (expr builder) t))
      | A.MatrixLit m -> (match (List.hd (List.hd m)) with
                                A.FloatLit _ -> let realOrder=List.map List.rev m in let i32Lists = List.map (List.map (expr builder)) realOrder in let listOfArrays=List.map Array.of_list i32Lists in let i32ListOfArrays = List.map (L.const_array float_t) listOfArrays in let arrayOfArrays=Array.of_list i32ListOfArrays in L.const_array (array_t float_t (List.length (List.hd m))) arrayOfArrays
                              | A.IntLit _ -> let realOrder=List.map List.rev m in let i32Lists = List.map (List.map (expr builder)) realOrder in let listOfArrays=List.map Array.of_list i32Lists in let i32ListOfArrays = List.map (L.const_array i32_t) listOfArrays in let arrayOfArrays=Array.of_list i32ListOfArrays in L.const_array (array_t i32_t (List.length (List.hd m))) arrayOfArrays
                              | A.TupleLit t -> let realOrder=List.map List.rev m in let i32Lists = List.map (List.map (expr builder)) realOrder in let listOfArrays=List.map Array.of_list i32Lists in let i32ListOfArrays = List.map (L.const_array (array_t (get_tuple_type t) (List.length t))) listOfArrays in let arrayOfArrays=Array.of_list i32ListOfArrays in L.const_array (array_t (array_t (get_tuple_type t) (List.length t)) (List.length (List.hd m))) arrayOfArrays
                              | _ -> raise ( UnsupportedMatrixType ))
      | A.RowReference (s) -> build_row_argument s builder
      | A.MatrixReference (s) -> build_matrix_argument s builder
      | A.RowLit r ->  L.const_array (get_row_type r) (Array.of_list (List.map (expr builder) r))
	    | A.MatrixAccess(s, e1, e2) -> let i1 = expr builder e1 and i2 = expr builder e2 in build_matrix_access s (L.const_int i32_t 0) i1 i2 builder false
      | A.PointerIncrement (s) -> build_pointer_increment s builder false
      | A.Dereference (s) -> build_pointer_dereference s builder false
	    | A.RowAccess(s, e1) -> let i1 = expr builder e1 in build_row_access s (L.const_int i32_t 0) i1 builder false
      | A.TupleAccess(s, e1) -> let i1 = expr builder e1 in build_tuple_access s (L.const_int i32_t 0) i1 builder false
      | A.MRowAccess(s, e1) -> let i1 = expr builder e1 in build_mrow_access s (L.const_int i32_t 0) i1 builder false 
      | A.MRowAccess(s, e1) -> let i1 = expr builder e1 in build_mrow_access s (L.const_int i32_t 0) i1 builder false
      | A.Length(s) -> L.const_int i32_t (get_length s)
      | A.Init(e1,e2) -> let cnt1=(lookup e1) and cnt2= expr builder e2 in
        let tp= L.element_type (L.type_of cnt1) in 
        let sz=L.size_of tp in
        let sz1=L.build_intcast sz (i32_t) "intc" builder in
        let dt=L.build_bitcast (L.build_call calloc_func [|cnt2;sz1|] "tmpa" builder) tp "tmpb" builder in
        L.build_store dt cnt1 builder
      | A.Binop (e1, op, e2) -> 
        let e1' = expr builder e1 and
        e2' = expr builder e2 and
        t1 = expr1 e1 and
        t2 = expr1 e2 in
          let float_bop operator = 
            (match operator with
              A.Add     -> L.build_fadd
            | A.Sub     -> L.build_fsub
            | A.Mult    -> L.build_fmul
            | A.Div     -> L.build_fdiv
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
            | A.Neq     -> L.build_fcmp L.Fcmp.One
            | A.Less    -> L.build_fcmp L.Fcmp.Olt
            | A.Leq     -> L.build_fcmp L.Fcmp.Ole
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt
            | A.Geq     -> L.build_fcmp L.Fcmp.Oge
            | _ -> raise (Failure("Unsupported operator"))
            ) e1' e2' "tmp" builder 
          in 

          let int_bop operator = 
            (match operator with
              A.Add     -> L.build_add
            | A.Sub     -> L.build_sub
            | A.Mult    -> L.build_mul
            | A.Div     -> L.build_sdiv
            | A.And     -> L.build_and
            | A.Or      -> L.build_or
            | A.Equal   -> L.build_icmp L.Icmp.Eq
            | A.Neq     -> L.build_icmp L.Icmp.Ne
            | A.Less    -> L.build_icmp L.Icmp.Slt
            | A.Leq     -> L.build_icmp L.Icmp.Sle
            | A.Greater -> L.build_icmp L.Icmp.Sgt
            | A.Geq     -> L.build_icmp L.Icmp.Sge
            | _ -> raise (Failure("Unsupported operator"))
            ) e1' e2' "tmp" builder
          in

          let tuple_int_bop n_i operator =
            let lhs_str = (match e1 with A.Id(s) -> s | _ -> "") in
            let rhs_str = (match e2 with A.Id(s) -> s | _ -> "") in
              (match operator with
                A.Add ->
                  let tmp_t = L.build_alloca (array_t i32_t n_i) "tmptup" builder in
                  for i=0 to n_i do
                    let v1 = build_tuple_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                    let v2 = build_tuple_access rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                    let add_res = L.build_add v1 v2 "tmp" builder in
                    let ld = L.build_gep tmp_t [| L.const_int i32_t 0; L.const_int i32_t i |] "tmptup" builder in
                  ignore(L.build_store add_res ld builder);
                  done;
                L.build_load (L.build_gep tmp_t [| L.const_int i32_t 0 |] "tmptup" builder) "tmptup" builder
                | A.Sub ->
                    let tmp_t = L.build_alloca (array_t i32_t n_i) "tmptup" builder in
                    for i=0 to n_i do
                      let v1 = build_tuple_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                      let v2 = build_tuple_access rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                      let add_res = L.build_sub v1 v2 "tmp" builder in
                      let ld = L.build_gep tmp_t [| L.const_int i32_t 0; L.const_int i32_t i |] "tmptup" builder in
                    ignore(L.build_store add_res ld builder);
                    done;
                  L.build_load (L.build_gep tmp_t [| L.const_int i32_t 0 |] "tmptup" builder) "tmptup" builder
                | _ -> raise (Failure("Unsupported operator")))
            in

          let tuple_int_scalar_bop n_i operator =
            let lhs_str = (match e1 with A.Id(s) -> s | _ -> "") in
            (* let rhs_str = (match e2 with A.Id(s) -> s | _ -> "") in *)
              (match operator with
                A.Add ->
                  let tmp_t = L.build_alloca (array_t i32_t n_i) "tmptup" builder in
                  for i=0 to n_i do
                    let v1 = build_tuple_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                    let add_res = L.build_add v1 e2' "tmp" builder in
                    let ld = L.build_gep tmp_t [| L.const_int i32_t 0; L.const_int i32_t i |] "tmptup" builder in
                  ignore(L.build_store add_res ld builder);
                  done;
                L.build_load (L.build_gep tmp_t [| L.const_int i32_t 0 |] "tmptup" builder) "tmptup" builder
                | A.Sub ->
                    let tmp_t = L.build_alloca (array_t i32_t n_i) "tmptup" builder in
                    for i=0 to n_i do
                      let v1 = build_tuple_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                      let add_res = L.build_sub v1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_t [| L.const_int i32_t 0; L.const_int i32_t i |] "tmptup" builder in
                    ignore(L.build_store add_res ld builder);
                    done;
                  L.build_load (L.build_gep tmp_t [| L.const_int i32_t 0 |] "tmptup" builder) "tmptup" builder
                | A.Mult ->
                    let tmp_t = L.build_alloca (array_t i32_t n_i) "tmptup" builder in
                    for i=0 to n_i do
                      let v1 = build_tuple_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                      let add_res = L.build_mul v1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_t [| L.const_int i32_t 0; L.const_int i32_t i |] "tmptup" builder in
                    ignore(L.build_store add_res ld builder);
                    done;
                  L.build_load (L.build_gep tmp_t [| L.const_int i32_t 0 |] "tmptup" builder) "tmptup" builder
                | A.Div ->
                    let tmp_t = L.build_alloca (array_t i32_t n_i) "tmptup" builder in
                    for i=0 to n_i do
                      let v1 = build_tuple_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                      let add_res = L.build_sdiv v1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_t [| L.const_int i32_t 0; L.const_int i32_t i |] "tmptup" builder in
                    ignore(L.build_store add_res ld builder);
                    done;
                  L.build_load (L.build_gep tmp_t [| L.const_int i32_t 0 |] "tmptup" builder) "tmptup" builder
                | _ -> raise (Failure("Unsupported operator")))
            in

          let int_tuple_scalar_bop n_i operator =
            let lhs_str = (match e2 with A.Id(s) -> s | _ -> "") in
              (match operator with
                A.Add ->
                  let tmp_t = L.build_alloca (array_t i32_t n_i) "tmptup" builder in
                  for i=0 to n_i do
                    let v1 = build_tuple_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                    let add_res = L.build_add e1' v1 "tmp" builder in
                    let ld = L.build_gep tmp_t [| L.const_int i32_t 0; L.const_int i32_t i |] "tmptup" builder in
                  ignore(L.build_store add_res ld builder);
                  done;
                L.build_load (L.build_gep tmp_t [| L.const_int i32_t 0 |] "tmptup" builder) "tmptup" builder
                | A.Sub ->
                    let tmp_t = L.build_alloca (array_t i32_t n_i) "tmptup" builder in
                    for i=0 to n_i do
                      let v1 = build_tuple_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                      let add_res = L.build_sub e1' v1 "tmp" builder in
                      let ld = L.build_gep tmp_t [| L.const_int i32_t 0; L.const_int i32_t i |] "tmptup" builder in
                    ignore(L.build_store add_res ld builder);
                    done;
                  L.build_load (L.build_gep tmp_t [| L.const_int i32_t 0 |] "tmptup" builder) "tmptup" builder
                | A.Mult ->
                    let tmp_t = L.build_alloca (array_t i32_t n_i) "tmptup" builder in
                    for i=0 to n_i do
                      let v1 = build_tuple_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                      let add_res = L.build_mul e1' v1 "tmp" builder in
                      let ld = L.build_gep tmp_t [| L.const_int i32_t 0; L.const_int i32_t i |] "tmptup" builder in
                    ignore(L.build_store add_res ld builder);
                    done;
                  L.build_load (L.build_gep tmp_t [| L.const_int i32_t 0 |] "tmptup" builder) "tmptup" builder
                | A.Div ->
                    let tmp_t = L.build_alloca (array_t i32_t n_i) "tmptup" builder in
                    for i=0 to n_i do
                      let v1 = build_tuple_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) builder false in
                      let add_res = L.build_sdiv e1' v1 "tmp" builder in
                      let ld = L.build_gep tmp_t [| L.const_int i32_t 0; L.const_int i32_t i |] "tmptup" builder in
                    ignore(L.build_store add_res ld builder);
                    done;
                  L.build_load (L.build_gep tmp_t [| L.const_int i32_t 0 |] "tmptup" builder) "tmptup" builder
                | _ -> raise (Failure("Unsupported operator")))
            in

          let matrix_int_bop r_i c_i operator =
            let lhs_str = (match e1 with A.Id(s) -> s | _ -> "") in
            let rhs_str = (match e2 with A.Id(s) -> s | _ -> "") in
              (match operator with
                A.Add ->
                  let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let m2 = build_matrix_access rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_add m1 m2 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Sub -> 
                  let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let m2 = build_matrix_access rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_sub m1 m2 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | _ -> raise (Failure("Unsupported operator")))
          in

          let matrix_int_scalar_bop r_i c_i operator =
            let lhs_str = (match e1 with A.Id(s) -> s | _ -> "") in
              (match operator with
                A.Add ->
                  let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_add m1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Sub -> 
                  let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_sub m1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Mult -> 
                  let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_mul m1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Div -> 
                  let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_sdiv m1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | _ -> raise (Failure("Unsupported operator")))
          in

          let int_matrix_scalar_bop r_i c_i operator =
            let lhs_str = (match e2 with A.Id(s) -> s | _ -> "") in
              (match operator with
                A.Add ->
                  let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_add e1' m1 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Sub -> 
                  let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_sub e1' m1 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Mult -> 
                  let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_mul e1' m1 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Div -> 
                  let tmp_m = L.build_alloca (array_t (array_t i32_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_sdiv e1' m1 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | _ -> raise (Failure("Unsupported operator")))
          in

          let matrix_float_bop r_i c_i operator =
            let lhs_str = (match e1 with A.Id(s) -> s | _ -> "") in
            let rhs_str = (match e2 with A.Id(s) -> s | _ -> "") in
              (match operator with
                A.Add ->
                  let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let m2 = build_matrix_access rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_fadd m1 m2 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Sub ->
                  let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let m2 = build_matrix_access rhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_fsub m1 m2 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | _ -> raise (Failure("Unsupported operator")))
          in

        let matrix_float_scalar_bop r_i c_i operator =
            let lhs_str = (match e1 with A.Id(s) -> s | _ -> "") in
              (match operator with
                A.Add ->
                  let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_fadd m1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Sub ->
                  let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_fsub m1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Mult ->
                  let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_fmul m1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Div ->
                  let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_fdiv m1 e2' "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | _ -> raise (Failure("Unsupported operator")))
          in

        let float_matrix_scalar_bop r_i c_i operator =
            let lhs_str = (match e2 with A.Id(s) -> s | _ -> "") in
              (match operator with
                A.Add ->
                  let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_fadd e1' m1 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Sub ->
                  let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_fsub e1' m1 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Mult ->
                  let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_fmul e1' m1 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | A.Div ->
                  let tmp_m = L.build_alloca (array_t (array_t float_t c_i) r_i) "tmpmat" builder in
                  for i=0 to (r_i-1) do
                    for j=0 to (c_i-1) do
                      let m1 = build_matrix_access lhs_str (L.const_int i32_t 0) (L.const_int i32_t i) (L.const_int i32_t j) builder false in
                      let add_res = L.build_fdiv e1' m1 "tmp" builder in
                      let ld = L.build_gep tmp_m [| L.const_int i32_t 0; L.const_int i32_t i; L.const_int i32_t j |] "tmpmat" builder in
                    ignore(L.build_store add_res ld builder);
                    done
                  done;
                L.build_load (L.build_gep tmp_m [| L.const_int i32_t 0 |] "tmpmat" builder) "tmpmat" builder
              | _ -> raise (Failure("Unsupported operator")))
          in

        let string_of_e1'_llvalue = L.string_of_llvalue e1'
        and string_of_e2'_llvalue = L.string_of_llvalue e2' in

        let space = Str.regexp " " in

        let list_of_e1'_llvalue = Str.split space string_of_e1'_llvalue
        and list_of_e2'_llvalue = Str.split space string_of_e2'_llvalue in

        let i32_re = Str.regexp "i32\\|i32*\\|i8\\|i8*\\|i1\\|i1*"
        and float_re = Str.regexp "double\\|double*" in

        let rec match_string regexp str_list i =
         let length = List.length str_list in
         match (Str.string_match regexp (List.nth str_list i) 0) with
           true -> true
         | false -> if (i > length - 2) then false else match_string regexp str_list (succ i) in

        let get_type llvalue =
           match (match_string i32_re llvalue 0) with
             true  -> "int"
           | false -> (match (match_string float_re llvalue 0) with
                         true -> "float"
                       | false -> "") in

        let e1'_type = get_type list_of_e1'_llvalue
        and e2'_type = get_type list_of_e2'_llvalue in

        let build_ops_with_types typ1 typ2 =
          match (typ1, typ2) with
            "int", "int" -> (match (t1, t2) with A.Int, A.Int -> int_bop op
                                      | A.TupleTyp(A.Int,l1),A.TupleTyp(A.Int,l2) when l1=l2->tuple_int_bop l1 op
                                      | A.TupleTyp(A.Int,l1), A.Int -> tuple_int_scalar_bop l1 op
                                      | A.Int, A.TupleTyp(A.Int,l1) -> int_tuple_scalar_bop l1 op 
                                      | A.MatrixTyp(A.Int,r1,c1),A.MatrixTyp(A.Int,r2,c2) when r1=r2 && c1=c2 -> matrix_int_bop r1 c1 op
                                      | A.MatrixTyp(A.Int,r1,c1), A.Int -> matrix_int_scalar_bop r1 c1 op 
                                      | A.Int, A.MatrixTyp(A.Int,r1,c1) -> int_matrix_scalar_bop r1 c1 op
                                      | _,_ -> raise (Failure("Cannot build ops with given types")))
          | "float" , "float" -> (match (t1,t2) with A.Float, A.Float -> float_bop op
                                        | A.MatrixTyp(A.Float,r1,c1),A.MatrixTyp(A.Float,r2,c2) when r1=r2 && c1=c2 -> matrix_float_bop r1 c1 op
                                        | A.MatrixTyp(A.Float,r1,c1), A.Float -> matrix_float_scalar_bop r1 c1 op 
                                        | A.Float, A.MatrixTyp(A.Float,r1,c1) -> float_matrix_scalar_bop r1 c1 op
                                        | _,_ -> raise (Failure("Cannot build ops with given types")))
          | _,_ -> raise(UnsupportedBinop)
        in
        build_ops_with_types e1'_type e2'_type
      | A.Unop(op, e) ->
        let e' = expr builder e in

        let float_uops operator =
          match operator with
            A.Neg -> L.build_fneg e' "tmp" builder
          | A.Not -> raise(UnsupportedUnop)  in

        let int_uops operator =
          match operator with
            A.Neg -> L.build_neg e' "tmp" builder
          | A.Not -> L.build_not e' "tmp" builder in

        let bool_uops operator = 
          match operator with
          A.Neg -> L.build_neg e' "tmp" builder
          | A.Not -> L.build_not e' "tmp" builder in

        let string_of_e'_llvalue = L.string_of_llvalue e' in

        let space = Str.regexp " " in

        let list_of_e'_llvalue = Str.split space string_of_e'_llvalue in

        let i32_re = Str.regexp "i32\\|i32*"
        and float_re = Str.regexp "double\\|double*"
        and bool_re = Str.regexp "i1\\|i1*" in

        let rec match_string regexp str_list i =
          let length = List.length str_list in
            match (Str.string_match regexp (List.nth str_list i) 0) with
              true -> true
            | false -> if (i > length - 2) then false else match_string regexp str_list (succ i) in

        let get_type llvalue =
          match (match_string i32_re llvalue 0) with
            true  -> "int"
          | false -> (match (match_string float_re llvalue 0) with
                  true -> "float"
                | false -> (match (match_string bool_re llvalue 0) with
                              true -> "bool"
                            | false -> "")) in

        let e'_type = get_type list_of_e'_llvalue  in

        let build_ops_with_type typ =
          match typ with
            "int" -> int_uops op
          | "float" -> float_uops op
          | "bool" -> bool_uops op
          | _ -> raise(UnsupportedUnop)
        in
        build_ops_with_type e'_type 
      | A.Assign (e1, e2) -> let e1' = (match e1 with
                                            A.Id s -> lookup s
										  | A.RowAccess(s, e1) -> let i1 = expr builder e1 in build_row_access s (L.const_int i32_t 0) i1 builder true
										  | A.TupleAccess(s, e1) -> let i1 = expr builder e1 in build_tuple_access s (L.const_int i32_t 0) i1 builder true
										  | A.MatrixAccess(s, e1, e2) -> let i1 = expr builder e1 and i2 = expr builder e2 in build_matrix_access s (L.const_int i32_t 0) i1 i2 builder true
										  | A.MRowAccess(s, e1) -> let i1 = expr builder e1 in build_mrow_access s (L.const_int i32_t 0) i1 builder true
                      | A.PointerIncrement(s) -> build_pointer_increment s builder true
                      | A.Dereference(s) -> build_pointer_dereference s builder true
                      | _ -> raise (IllegalAssignment))
                             and e2' = expr builder e2 in
                     ignore (L.build_store e2' e1' builder); e2' 
      | A.Call("open", e) -> 
    L.build_call open_func (Array.of_list (List.rev (List.map (expr builder) (List.rev e)))) "fopen" builder
      | A.Call("close", e) -> 
    L.build_call close_func (Array.of_list (List.rev (List.map (expr builder) (List.rev e)))) "fclose" builder
      | A.Call("write", e) -> 
    L.build_call fwrite_func (Array.of_list (List.rev (List.map (expr builder) (List.rev e)))) "tmpy" builder 
      | A.Call("read", e) -> 
    L.build_call read_func (Array.of_list (List.rev (List.map (expr builder) (List.rev e)))) "tmpx" builder
      | A.Call("fget", e) -> 
    L.build_call get_func (Array.of_list (List.rev (List.map (expr builder) (List.rev e)))) "tmpz" builder
      | A.Call("len", e) -> 
    L.build_call strlen_func (Array.of_list (List.rev (List.map (expr builder) (List.rev e)))) "len" builder
      | A.Call ("atoi", e) ->
    L.build_call cast_str_int_func (Array.of_list (List.rev (List.map (expr builder) (List.rev e)))) "tmp3" builder 
      | A.Call ("itos", e) ->
    L.build_call sprintf_func (Array.of_list (List.rev (List.map (expr builder) (List.rev e)))) "sprintf" builder 
      | A.Call("splitstr", e)->
     L.build_call strtok_fun (Array.of_list (List.rev (List.map (expr builder) (List.rev e)))) "tmp5" builder 
      | A.Call ("print", [e]) | A.Call ("printb", [e]) ->
    L.build_call printf_func [| int_format_str ; (expr builder e) |]
      "printf" builder
      | A.Call ("printf", [e]) ->
    L.build_call printf_func [| float_format_str ; (expr builder e) |]
      "printf" builder
      | A.Call("print_c",[e]) ->
    L.build_call printf_func [| char_format_str; (expr builder e)|]
      "printf" builder
      | A.Call("prints",[e]) -> 
    L.build_call prints_func [|(expr builder e)|] 
      "puts" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
   let actuals = List.rev (List.map (expr builder) (List.rev act)) in
   let result = (match fdecl.A.typ with A.Void -> ""
                                            | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
  Some _ -> ()

      | None -> ignore (f builder) in
  
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
        A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder e); builder 
      | A.Return e -> ignore (match fdecl.A.typ with
         A.Void -> L.build_ret_void builder
        | _ -> L.build_ret (expr builder e) builder); builder
        | A.If (predicate, then_stmt, else_stmt) ->
           let bool_val = expr builder predicate in
  let merge_bb = L.append_block context "merge" the_function in

   let then_bb = L.append_block context "then" the_function in
   add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
     (L.build_br merge_bb);

   let else_bb = L.append_block context "else" the_function in
   add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
     (L.build_br merge_bb);

   ignore (L.build_cond_br bool_val then_bb else_bb builder);
   L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
    let pred_bb = L.append_block context "while" the_function in
    ignore (L.build_br pred_bb builder);

    let body_bb = L.append_block context "while_body" the_function in
    add_terminal (stmt (L.builder_at_end context body_bb) body)
      (L.build_br pred_bb);

    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

    let merge_bb = L.append_block context "merge" the_function in
    ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
    L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
      ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )


	  | A.MFor (s1, s2, body) ->
		let rows = L.array_length (L.type_of (L.build_load (L.build_gep (lookup s2) [| L.const_int i32_t 0 |] s2 builder) s2 builder)) in
		let cols = L.array_length (L.type_of (L.build_load (L.build_gep (lookup s2) [| L.const_int i32_t 0; L.const_int i32_t 0 |] s2 builder) s2 builder)) in
		allocate_row s1 cols s2; 
		(*let print_vars key value = print_endline(key) in
		StringMap.iter print_vars !local_vars;	*)

		stmt builder
		  ( A.Block
			[A.Expr (A.Assign(A.Id "_i", A.IntLit 0));
			 A.While ((A.Binop((A.Id "_i"), A.Less, (A.IntLit rows))),
			   A.Block [A.Expr (A.Assign((A.Id s1), A.MRowAccess(s2, (A.Id "_i")))); body ; A.Expr(A.Assign((A.Id "_i"), A.Binop((A.Id "_i"), A.Add, (A.IntLit 1))))]) ] )
	in


    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | A.Int -> L.build_ret (L.const_int i32_t 0)
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | A.Bool -> L.build_ret (L.const_int i1_t 0)
     (*
      | A.RowTyp(t) -> (match t with
                          A.Int -> L.build_ret(L.const_pointer_null (pointer_t i32_t))
                        | A.Float -> L.build_ret (L.const_pointer_null (pointer_t float_t))
                        | _ -> raise (UnsupportedReturnType))
      | A.MatrixTyp(t) -> (match t with
                            A.Int -> L.build_ret (L.const_pointer_null (pointer_t i32_t))
                          | A.Float -> L.build_ret (L.const_pointer_null (pointer_t float_t))
                          | _ -> raise (UnsupportedReturnType))
      | A.MatrixPointer(t) -> (match t with
                                  A.Int -> L.build_ret (L.const_pointer_null (pointer_t i32_t))
                                | A.Float -> L.build_ret (L.const_pointer_null (pointer_t float_t))
                                | _ -> raise (UnsupportedReturnType)) *)
      | _ -> raise (UnsupportedReturnType))
  in

  List.iter build_function_body functions;
  the_module
