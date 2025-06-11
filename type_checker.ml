open Ast

(* Type definitions *)
type data_type = 
  | TInt
  | TFloat
  | TBool
  | TVector of int (* dimension *)
  | TMatrix of int * int (* rows * columns *)
  | TUnknown

(* Error type *)
type type_error =
  | TypeMismatch of string * data_type * data_type
  | UndefinedVariable of string
  | InvalidOperation of string
  | DimensionMismatch of string
  | Other of string


let read_matrix_dimensions filename =
    let filepath = Printf.sprintf "%d.txt" filename in
    try
      let ic = open_in filepath in
      let rec count_rows cols rows =
        try
          let line = input_line ic in
          let current_cols = List.length (String.split_on_char ' ' line) in
          if cols = -1 then count_rows current_cols (rows + 1)
          else if current_cols = cols then count_rows cols (rows + 1)
          else raise (Failure "Inconsistent matrix dimensions")
        with End_of_file -> (rows, cols)
      in
      let (rows, cols) = count_rows (-1) 0 in
      close_in ic;
      (rows, cols)
    with
    | Sys_error err -> raise (Failure ("File error: " ^ err))
    | Failure msg -> raise (Failure msg)
(* Environment to track variable types *)
type environment = (string * data_type) list

(* Helper functions *)
let string_of_type = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TVector n -> Printf.sprintf "vector(%d)" n
  | TMatrix (r, c) -> Printf.sprintf "matrix(%d,%d)" r c
  | TUnknown -> "unknown"

(* Find variable type in environment *)
let rec lookup env var =
  match env with
  | [] -> None
  | (name, typ)::rest -> if name = var then Some typ else lookup rest var

(* Update or add variable type in environment *)
let update env var typ =
  (var, typ) :: List.filter (fun (name, _) -> name <> var) env

(* Check if types are compatible *)
let compatible t1 t2 =
  match t1, t2 with
  | TInt, TFloat | TFloat, TInt -> true
  | TVector n1, TVector n2 -> n1 = n2
  | TMatrix (r1, c1), TMatrix (r2, c2) -> r1 = r2 && c1 = c2
  | t1, t2 -> t1 = t2

(* Type checking expressions *)
let rec type_check_expr env expr =
  match expr with
  | IntLit _ -> Ok (TInt, env)
  | FloatLit _ -> Ok (TFloat, env)
  | BoolLit _ -> Ok (TBool, env)
  | Var name -> 
      begin match lookup env name with
      | Some typ -> Ok (typ, env)
      | None -> Error (UndefinedVariable name)
      end
  | ParenExpr e -> type_check_expr env e
  | Assign (Var name, e) ->
      begin match type_check_expr env e with
      | Ok (t, env') -> Ok (t, update env' name t)
      | Error e -> Error e
      end
  | Assign (_, _) -> Error (Other "Left side of assignment must be a variable")
  | BinOp (e1, op, e2) -> type_check_binop env e1 op e2
  | UnaryOp (op, e) -> type_check_unaryop env op e
  | VectorLiteral values -> 
      Ok (TVector (List.length values), env)
  | MatrixLiteral rows ->
      if List.length rows = 0 then
        Error (Other "Empty matrix is not allowed")
      else
        let row_lengths = List.map List.length rows in
        if List.for_all (fun len -> len = List.hd row_lengths) row_lengths then
          Ok (TMatrix (List.length rows, List.hd row_lengths), env)
        else
          Error (Other "Matrix rows must have the same length")
  | VectorIndex (name, idx) ->
      begin match lookup env name, type_check_expr env idx with
      | Some (TVector _), Ok (TInt, _) -> Ok (TFloat, env)
      | Some (TVector _), Ok (t, _) -> 
          Error (TypeMismatch ("Vector index", TInt, t))
      | Some t, _ -> 
          Error (TypeMismatch ("Vector access", TVector 0, t))
      | None, _ -> 
          Error (UndefinedVariable name)
      end
  | MatrixIndex (name, row, col) ->
      begin match lookup env name, type_check_expr env row, type_check_expr env col with
      | Some (TMatrix _), Ok (TInt, _), Ok (TInt, _) -> Ok (TFloat, env)
      | Some (TMatrix _), Ok (r, _), Ok (c, _) -> 
          if r <> TInt then Error (TypeMismatch ("Matrix row index", TInt, r))
          else Error (TypeMismatch ("Matrix column index", TInt, c))
      | Some t, _, _ -> Error (TypeMismatch ("Matrix access", TMatrix (0, 0), t))
      | None, _, _ -> Error (UndefinedVariable name)
      end
  | MatrixRowAccess (name, row) ->
      begin match lookup env name, type_check_expr env row with
      | Some (TMatrix (_, cols)), Ok (TInt, _) -> Ok (TVector cols, env)
      | Some (TMatrix _), Ok (t, _) -> 
          Error (TypeMismatch ("Matrix row index", TInt, t))
      | Some t, _ -> 
          Error (TypeMismatch ("Matrix row access", TMatrix (0, 0), t))
      | None, _ -> 
          Error (UndefinedVariable name)
      end
  | BuiltInFunc (name, args) -> type_check_builtin_func env name args
  (* Type checking binary operations *)
and type_check_binop env e1 op e2 =
  match type_check_expr env e1, type_check_expr env e2 with
  | Ok (t1, _), Ok (t2, _) ->
      begin match op with
      | Plus | Minus | Mult | Div ->
          begin match t1, t2 with
          | TInt, TInt -> Ok (TInt, env)
          | TFloat, TFloat | TInt, TFloat | TFloat, TInt -> Ok (TFloat, env)
          | _, _ -> Error (InvalidOperation (Printf.sprintf "Cannot apply '%s' to %s and %s" 
                                             (match op with Plus -> "+" | Minus -> "-" | Mult -> "*" | Div -> "/" | _ -> "")
                                             (string_of_type t1) (string_of_type t2)))
          end
      | Mod | Pow ->
          begin match t1, t2 with
          | TInt, TInt -> Ok (TInt, env)
          | TFloat, TFloat | TInt, TFloat | TFloat, TInt -> 
              if op = Mod then 
                Error (InvalidOperation "Modulo operation requires integer operands")
              else 
                Ok (TFloat, env)
          | _, _ -> Error (InvalidOperation (Printf.sprintf "Cannot apply '%s' to %s and %s" 
                                             (if op = Mod then "%" else "**")
                                             (string_of_type t1) (string_of_type t2)))
          end
      | Eq | Neq | Lt | Gt | Lte | Gte ->
          if compatible t1 t2 then Ok (TBool, env)
          else Error (TypeMismatch ("Comparison", t1, t2))
      | And | Or ->
          if t1 = TBool && t2 = TBool then Ok (TBool, env)
          else Error (InvalidOperation (Printf.sprintf "Logical operation requires boolean operands, got %s and %s" 
                                       (string_of_type t1) (string_of_type t2)))
      end
  | Error e, _ -> Error e
  | _, Error e -> Error e

(* Type checking unary operations *)
and type_check_unaryop env op e =
  match type_check_expr env e with
  | Ok (t, _) ->
      begin match op, t with
      | Not, TBool -> Ok (TBool, env)
      | Not, _ -> Error (TypeMismatch ("logical not", TBool, t))
      end
  | Error e -> Error e
  (* Type checking built-in functions *)
and type_check_builtin_func env name args =
  match name, args with
  | "create", [e1; e2] ->
      begin match type_check_expr env e1, type_check_expr env e2 with
      | Ok (TVector n, _), Ok (TFloat, _) -> Ok (TVector (n), env) (* Assuming dimension from first arg *)
      | Ok (t1, _), Ok (t2, _) -> 
          Error (InvalidOperation (Printf.sprintf "create expects (int, float), got (%s, %s)" 
                                 (string_of_type t1) (string_of_type t2)))
      | Error e, _ | _, Error e -> Error e
      end
  | "dim", [e] ->
      begin match type_check_expr env e with
      | Ok (TVector _, _) -> Ok (TInt, env)
      | Ok (t, _) -> Error (TypeMismatch ("dimension", TVector 0, t))
      | Error e -> Error e
      end
  | "is_zero", [e] ->
      begin match type_check_expr env e with
      | Ok (TVector _, _) -> Ok (TBool, env)
      | Ok (t, _) -> Error (TypeMismatch ("is_zero", TVector 0, t))
      | Error e -> Error e
      end
  | "unit", [e] ->
      begin match type_check_expr env e with
      | Ok ( TVector n, _) -> Ok (TVector n, env)
      | Ok (t, _) -> Error (TypeMismatch ("unit", TVector 0, t))
      | Error e -> Error e
      end
  | "scale", [e1; e2] ->
      begin match type_check_expr env e1, type_check_expr env e2 with
      | Ok (TFloat, _), Ok (TVector n, _) | Ok (TInt, _), Ok (TVector n, _) -> 
          Ok (TVector n, env)
      | Ok (t1, _), Ok (t2, _) -> 
          Error (InvalidOperation (Printf.sprintf "scale expects (float/int, vector), got (%s, %s)" 
                                 (string_of_type t1) (string_of_type t2)))
      | Error e, _ | _, Error e -> Error e
      end
  | "addv", [e1; e2] ->
      begin match type_check_expr env e1, type_check_expr env e2 with
      | Ok (TVector n1, _), Ok (TVector n2, _) -> 
          if n1 = n2 then Ok (TVector n1, env)
          else Error (DimensionMismatch (Printf.sprintf "Vector dimensions don't match: %d and %d" n1 n2))
      | Ok (t1, _), Ok (t2, _) -> 
          Error (InvalidOperation (Printf.sprintf "addv expects (vector, vector), got (%s, %s)" 
                                 (string_of_type t1) (string_of_type t2)))
      | Error e, _ | _, Error e -> Error e
      end
  | "dot_prod", [e1; e2] ->
      begin match type_check_expr env e1, type_check_expr env e2 with
      | Ok (TVector n1, _), Ok (TVector n2, _) -> 
          if n1 = n2 then Ok (TFloat, env)
          else Error (DimensionMismatch (Printf.sprintf "Vector dimensions don't match: %d and %d" n1 n2))
      | Ok (t1, _), Ok (t2, _) -> 
          Error (InvalidOperation (Printf.sprintf "dot_prod expects (vector, vector), got (%s, %s)" 
                                 (string_of_type t1) (string_of_type t2)))
      | Error e, _ | _, Error e -> Error e
      end
  | "inv", [e] ->
      begin match type_check_expr env e with
      | Ok (TVector n, _) -> Ok (TVector n, env)
      | Ok (t, _) -> Error (TypeMismatch ("inv", TVector 0, t))
      | Error e -> Error e
      end
  | "length", [e] ->
      begin match type_check_expr env e with
      | Ok (TVector _, _) -> Ok (TFloat, env)
      | Ok (t, _) -> Error (TypeMismatch ("length", TVector 0, t))
      | Error e -> Error e
      end
  | "angle", [e1; e2] ->
      begin match type_check_expr env e1, type_check_expr env e2 with
      | Ok (TVector n1, _), Ok (TVector n2, _) -> 
          if n1 = n2 then Ok (TFloat, env)
          else Error (DimensionMismatch (Printf.sprintf "Vector dimensions don't match: %d and %d" n1 n2))
      | Ok (t1, _), Ok (t2, _) -> 
          Error (InvalidOperation (Printf.sprintf "angle expects (vector, vector), got (%s, %s)" 
                                 (string_of_type t1) (string_of_type t2)))
      | Error e, _ | _, Error e -> Error e
      end
  | "row_shift", [e1; e2] ->
      begin match type_check_expr env e1, type_check_expr env e2 with
      | Ok (TMatrix (r, c), _), Ok (TInt, _) -> Ok (TMatrix (r, c), env)
      | Ok (t1, _), Ok (t2, _) -> Error (InvalidOperation (Printf.sprintf "row_shift expects (matrix, int), got (%s, %s)" 
      (string_of_type t1) (string_of_type t2)))
| Error e, _ | _, Error e -> Error e
end
| "row_sub", [e1; e2] ->
begin match type_check_expr env e1, type_check_expr env e2 with
| Ok (TMatrix (r, c), _), Ok (TInt, _) -> Ok (TMatrix (r, c), env)
| Ok (t1, _), Ok (t2, _) -> 
Error (InvalidOperation (Printf.sprintf "row_sub expects (matrix, int), got (%s, %s)" 
      (string_of_type t1) (string_of_type t2)))
| Error e, _ | _, Error e -> Error e
end
| "num_rows", [e] ->
begin match type_check_expr env e with
| Ok (TMatrix (r, _), _) -> Ok (TInt, env)
| Ok (t, _) -> Error (TypeMismatch ("num_rows", TMatrix (0, 0), t))
| Error e -> Error e
end
| "num_cols", [e] ->
begin match type_check_expr env e with
| Ok (TMatrix (_, c), _) -> Ok (TInt, env)
| Ok (t, _) -> Error (TypeMismatch ("num_cols", TMatrix (0, 0), t))
| Error e -> Error e
end
| "create_matrix", [e1; e2; e3] ->
begin match type_check_expr env e1, type_check_expr env e2, type_check_expr env e3 with
| Ok (TMatrix(m,n), _), Ok (TInt, _), Ok (TFloat, _) -> 
Ok (TMatrix (m,n ), env) (* Assuming dimensions from first two args *)
| Ok (t1, _), Ok (t2, _), Ok (t3, _) -> 
Error (InvalidOperation (Printf.sprintf "create_matrix expects (int, int, int), got (%s, %s, %s)" 
      (string_of_type t1) (string_of_type t2) (string_of_type t3)))
| Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
end
| "matrix_add", [e1; e2] ->
begin match type_check_expr env e1, type_check_expr env e2 with
| Ok (TMatrix (r1, c1), _), Ok (TMatrix (r2, c2), _) -> 
if r1 = r2 && c1 = c2 then Ok (TMatrix (r1, c1), env)
else Error (DimensionMismatch (Printf.sprintf "Matrix dimensions don't match: (%d,%d) and (%d,%d)" r1 c1 r2 c2))
| Ok (t1, _), Ok (t2, _) -> 
Error (InvalidOperation (Printf.sprintf "matrix_add expects (matrix, matrix), got (%s, %s)" 
      (string_of_type t1) (string_of_type t2)))
| Error e, _ | _, Error e -> Error e
end
| "matrix_transpose", [e] ->
begin match type_check_expr env e with
| Ok (TMatrix (r, c), _) -> Ok (TMatrix (c, r), env)
| Ok (t, _) -> Error (TypeMismatch ("matrix_transpose", TMatrix (0, 0), t))
| Error e -> Error e
end
| "matrix_determinant", [e] ->
begin match type_check_expr env e with
| Ok (TMatrix (r, c), _) -> 
if r = c then Ok (TFloat, env)
else Error (DimensionMismatch (Printf.sprintf "Matrix must be square for determinant, got (%d,%d)" r c))
| Ok (t, _) -> Error (TypeMismatch ("matrix_determinant", TMatrix (0, 0), t))
| Error e -> Error e
end
| "matrix_scal_mult", [e1; e2] ->
begin match type_check_expr env e1, type_check_expr env e2 with
|Ok (TMatrix (r, c), _), Ok (TFloat, _)  |  Ok (TMatrix (r, c), _),Ok (TInt, _) -> 
Ok (TMatrix (r, c), env)
| Ok (t1, _), Ok (t2, _) -> 
Error (InvalidOperation (Printf.sprintf "matrix_scal_mult expects (float/int, matrix), got (%s, %s)" 
      (string_of_type t1) (string_of_type t2)))
| Error e, _ | _, Error e -> Error e
end
| "matrix_minor", [e1; e2; e3] ->
begin match type_check_expr env e1, type_check_expr env e2, type_check_expr env e3 with
| Ok (TMatrix (r, c), _), Ok (TInt, _), Ok (TInt, _) -> 
if r = c then Ok (TMatrix (r-1, c-1), env)
else Error (DimensionMismatch (Printf.sprintf "Matrix must be square for minor, got (%d,%d)" r c))
| Ok (t1, _), Ok (t2, _), Ok (t3, _) -> 
Error (InvalidOperation (Printf.sprintf "matrix_minor expects (matrix, int, int), got (%s, %s, %s)" 
      (string_of_type t1) (string_of_type t2) (string_of_type t3)))
      | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
    end
| "matrix_mult", [e1; e2] ->
    begin match type_check_expr env e1, type_check_expr env e2 with
    | Ok (TMatrix (r1, c1), _), Ok (TMatrix (r2, c2), _) -> 
        if c1 = r2 then Ok (TMatrix (r1, c2), env)
        else Error (DimensionMismatch (Printf.sprintf "Matrix multiplication dimensions don't match: (%d,%d) and (%d,%d)" r1 c1 r2 c2))
    | Ok (t1, _), Ok (t2, _) -> 
        Error (InvalidOperation (Printf.sprintf "matrix_mult expects (matrix, matrix), got (%s, %s)" 
                               (string_of_type t1) (string_of_type t2)))
    | Error e, _ | _, Error e -> Error e
    end
|"update_matrix",[e1;e2;e3;e4] ->
    begin match type_check_expr env e1, type_check_expr env e2, type_check_expr env e3, type_check_expr env e4 with
    | Ok (TMatrix (r1,c1),_), Ok (TInt,_), Ok(TInt,_), Ok(TInt,_) ->
        (* If all types are correct, return the matrix type *)
        Ok (TMatrix (r1,c1), env)
    | Ok (TMatrix (r1,c1),_), Ok (TInt,_), Ok(TInt,_), Ok(TFloat,_) ->
        (* If all types are correct, return the matrix type *)
        Ok (TMatrix (r1,c1), env)
    | Ok (TMatrix _, _), Ok (t2, _), Ok (t3, _), Ok(t4,_) ->
        Error (InvalidOperation (Printf.sprintf "Expected (matrix, int, int, int/float) but got (matrix, %s, %s, %s)" 
               (string_of_type t2) (string_of_type t3) (string_of_type t4)))
    | Ok (t1, _), _, _, _ ->
        Error (InvalidOperation (Printf.sprintf "Expected matrix as first argument but got %s" (string_of_type t1)))
    | Error e, _, _, _ -> Error e
   
    end
| "input", [e] -> 
   ( match args with
        [Var "f"] -> Ok (TFloat, env)
     | [Var "i"] ->  Ok (TInt, env)
     | [Var "v"; e2] ->
       ( begin match type_check_expr env e2 with
        | Ok (TInt, _) -> 
            Ok (TVector 0, env)  (* Dimension will be known at runtime *)
        | Ok (t, _) -> 
            Error (TypeMismatch ("vector dimension", TInt, t))
        | Error e -> Error e
        end)
    | [Var "m"; e2; e3] ->
           ( begin match type_check_expr env e2, type_check_expr env e3 with
            | Ok (TInt, _), Ok (TInt, _) -> 
                Ok (TMatrix (0, 0), env)  (* Dimensions will be known at runtime *)
            | Ok (t1, _), Ok (TInt, _) -> 
                Error (TypeMismatch ("matrix rows", TInt, t1))
            | Ok (TInt, _), Ok (t2, _) -> 
                Error (TypeMismatch ("matrix columns", TInt, t2))
            | Ok (t1, _), Ok (t2, _) ->
                Error (InvalidOperation (Printf.sprintf 
                    "Matrix dimensions must be integers, got (%s, %s)" 
                    (string_of_type t1) (string_of_type t2)))
            | Error e, _ | _, Error e -> Error e
            end)
    | [e] -> ( begin match type_check_expr env e with
    | Ok (TInt, _) ->
        let (rows, cols) = read_matrix_dimensions (match e with IntLit i -> i | _ -> -1) in
        Ok (TMatrix (rows, cols), env)
    | Ok (t, _) -> Error (TypeMismatch ("input function expects an integer filename", TInt, t))
    | Error e -> Error e
    end
   )
   | _ -> Error (InvalidOperation "Invalid input format or wrong number of arguments")
   )

| "print", [e] ->
    begin match type_check_expr env e with
    | Ok (_, _) -> Ok (TBool, env) (* Assuming print returns success status *)
    | Error e -> Error e
    end
| _, _ -> Error (Other (Printf.sprintf "Unknown function or incorrect number of arguments: %s" name))

(* Type checking statements *)
let rec type_check_stmt env stmt =
match stmt with
| ExprStmt e -> 
    begin match type_check_expr env e with
    | Ok (_, env') -> Ok env'
    | Error e -> Error e
    end
| ReturnStmt e ->
    begin match type_check_expr env e with
    | Ok (_, env') -> Ok env'
    | Error e -> Error e
    end
| IfStatement (cond, then_stmts, else_stmts_opt) ->
    begin match type_check_expr env cond with
    | Ok (TBool, env') ->
        begin match type_check_stmts env' then_stmts with
        | Ok env_then ->
            begin match else_stmts_opt with
            | Some else_stmts -> type_check_stmts env_then else_stmts
            | None -> Ok env_then
            end
        | Error e -> Error e
        end
    | Ok (t, _) -> Error (TypeMismatch ("if condition", TBool, t))
    | Error e -> Error e
    end
| WhileStatement (cond, body) ->
    begin match type_check_expr env cond with
    | Ok (TBool, env') -> type_check_stmts env' body
    | Ok (t, _) -> Error (TypeMismatch ("while condition", TBool, t))
    | Error e -> Error e
    end
| ForStmt (var, start, end_, body) ->
    begin match type_check_expr env start, type_check_expr env end_ with
    | Ok (TInt, _), Ok (TInt, _) ->
        let env' = update env var TInt in
        type_check_stmts env' body
    | Ok (t1, _), Ok (t2, _) ->
        if t1 <> TInt then Error (TypeMismatch ("for loop start", TInt, t1))
        else Error (TypeMismatch ("for loop end", TInt, t2))
    | Error e, _ | _, Error e -> Error e
    end
| RaiseError -> Ok env

(* Type checking a list of statements *)
and type_check_stmts env stmts =
match stmts with
| [] -> Ok env
| stmt :: rest ->
    match type_check_stmt env stmt with
    | Ok env' -> type_check_stmts env' rest
    | Error e -> Error e

(* Type checking a program *)
let type_check_prog prog =
type_check_stmts [] prog

(* Error message formatting *)
let string_of_type_error = function
| TypeMismatch (ctx, expected, actual) ->
    Printf.sprintf "Type error in %s: expected %s, got %s" ctx (string_of_type expected) (string_of_type actual)
| UndefinedVariable name ->
    Printf.sprintf "Undefined variable: %s" name
| InvalidOperation msg ->
    Printf.sprintf "Invalid operation: %s" msg
| DimensionMismatch msg ->
    Printf.sprintf "Dimension mismatch: %s" msg
| Other msg ->
    Printf.sprintf "Type error: %s" msg