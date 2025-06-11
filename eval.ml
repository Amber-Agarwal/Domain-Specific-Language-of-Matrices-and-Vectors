open Ast

(* Evaluation Environment and Runtime *)
module Env = Map.Make(String)

(* Runtime values *)
type runtime_value = 
  | IntVal of int
  | FloatVal of float
  | BoolVal of bool
  | VectorVal of float list
  | MatrixVal of float list list
  | UnitVal  (* For functions that don't return a specific value *)

(* Custom exception for runtime errors *)
exception RuntimeError of string

(* Evaluation context to manage variables, functions, etc. *)
type context = {
  mutable variables: runtime_value Env.t;
}

(* Create a global context *)
let global_ctx = { variables = Env.empty }

(* Helper functions for type checking and conversions *)
let to_float = function
  | IntVal i -> float_of_int i
  | FloatVal f -> f
  | _ -> raise (RuntimeError "Cannot convert to float")

let to_int = function
  | IntVal i -> i
  | FloatVal f -> int_of_float f
  | _ -> raise (RuntimeError "Cannot convert to int")

(* Vector and Matrix Operations *)
let rec add_vectors v1 v2 =
  match (v1, v2) with
  | [], [] -> []
  | x::xs, y::ys -> (( x) +. ( y)) :: add_vectors xs ys
  | _ -> raise (RuntimeError "Vectors must have same length for addition")

let dot_product v1 v2 =
  List.fold_left2 (fun acc x y -> acc +. (( x) *. ( y))) 0.0 v1 v2

let create_unit_vector n =
  List.init n (fun i -> if i = 0 then 1.0 else 0.0)

let create_vector n v=
  List.init n (fun i -> v)



let vector_length v =
  sqrt (List.fold_left (fun acc x -> acc +. ((x) *. ( x))) 0.0 v)

let vector_angle v1 v2 =
  let dot = dot_product v1 v2 in
  let len1 = vector_length v1 in
  let len2 = vector_length v2 in
  acos (dot /. (len1 *. len2))

(* Matrix Operations *)
let matrix_add m1 m2 =
  List.map2 (fun row1 row2 -> List.map2 (+.) row1 row2) m1 m2

let matrix_transpose m =
  let rec transpose = function
    | [] -> []
    | []::_ -> []
    | rows -> 
        List.map List.hd rows :: transpose (List.map List.tl rows)
  in
  transpose m

let rec matrix_determinant m =
  (* Validate that matrix is square *)
  let rows = List.length m in
  let is_square = List.for_all (fun row -> List.length row = rows) m in
  
  if not is_square then 
    raise (RuntimeError "Determinant requires a square matrix")
  else
    (* Base cases *)
    match rows with
    | 0 -> 1.0  (* Empty matrix determinant is 1 *)
    | 1 -> List.hd (List.hd m)  (* 1x1 matrix *)
    | 2 -> 
        (* Optimized 2x2 matrix determinant *)
        (match m with
     | [[a;b];[c;d]] -> a *. d -. b *. c
     | _ -> raise (RuntimeError "Invalid 2x2 matrix structure"))
    | _ ->
        (* Laplace expansion along the first row *)
        List.mapi (fun j coeff ->
          let sign = if j mod 2 = 0 then 1.0 else -1.0 in
          let submatrix = 
            List.filteri (fun i _ -> i > 0) m  (* Remove first row *)
            |> List.map (fun row -> 
                List.filteri (fun k _ -> k <> j) row)  (* Remove current column *)
          in
          sign *. coeff *. matrix_determinant submatrix
        ) (List.hd m)
        |> List.fold_left (+.) 0.0

let matrix_minor m row col =
  let filtered_rows = 
    List.filteri (fun i _ -> i <> row) m 
    |> List.map (fun row -> 
        List.filteri (fun j _ -> j <> col) row)
  in
  filtered_rows

let matrix_multiply m1 m2 =
  let m2_transposed = matrix_transpose m2 in
  List.map (fun row -> 
    List.map (fun col -> 
      List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 row col
    ) m2_transposed
  ) m1

(* Modified evaluation function to use global context and only return values *)
let rec eval_expr ctx = function
  | IntLit i -> IntVal i
  | FloatLit f -> FloatVal f
  | BoolLit b -> BoolVal b
  | Var name -> 
      (try Env.find name ctx.variables 
       with Not_found -> raise (RuntimeError ("Undefined variable: " ^ name)))
  
  | Assign (Var name, expr) ->
      let value = eval_expr ctx expr in
      ctx.variables <- Env.add name value ctx.variables;
      value
  
  | BinOp (e1, op, e2) ->
      let v1 = eval_expr ctx e1 in
      let v2 = eval_expr ctx e2 in
      (match (v1, op, v2) with
      (* Handle mixed arithmetic by converting IntVal to FloatVal when needed *)
      | (IntVal a, Plus, FloatVal b) -> FloatVal ((float_of_int a) +. b)
      | (FloatVal a, Plus, IntVal b) -> FloatVal (a +. (float_of_int b))
      | (IntVal a, Minus, FloatVal b) -> FloatVal ((float_of_int a) -. b)
      | (FloatVal a, Minus, IntVal b) -> FloatVal (a -. (float_of_int b))
      | (IntVal a, Mult, FloatVal b) -> FloatVal ((float_of_int a) *. b)
      | (FloatVal a, Mult, IntVal b) -> FloatVal (a *. (float_of_int b))
      | (IntVal a, Div, FloatVal b) -> FloatVal ((float_of_int a) /. b)
      | (FloatVal a, Div, IntVal b) -> FloatVal (a /. (float_of_int b))
      
      (* Existing cases for same-type operations *)
      | (IntVal a, Plus, IntVal b) -> IntVal (a + b)
      | (FloatVal a, Plus, FloatVal b) -> FloatVal (a +. b)
      | (IntVal a, Minus, IntVal b) -> IntVal (a - b)
      | (FloatVal a, Minus, FloatVal b) -> FloatVal (a -. b)
      | (IntVal a, Mult, IntVal b) -> IntVal (a * b)
      | (FloatVal a, Mult, FloatVal b) -> FloatVal (a *. b)
      | (IntVal a, Div, IntVal b) -> IntVal (a / b)
      | (FloatVal a, Div, FloatVal b) -> FloatVal (a /. b)
      | (IntVal a, Mod, IntVal b) -> IntVal (a mod b)
      (* POWER operator cases *)
      | (FloatVal a, Pow, FloatVal b) -> FloatVal (a ** b)
      | (IntVal a, Pow, IntVal b) -> FloatVal ((float_of_int a) ** (float_of_int b))
      | (IntVal a, Pow, FloatVal b) -> FloatVal ((float_of_int a) ** b)
      | (FloatVal a, Pow, IntVal b) -> FloatVal (a ** (float_of_int b))
      
      (* Comparison & logical operations remain unchanged *)
      | (IntVal a, Eq, IntVal b) -> BoolVal (a = b)
      | (FloatVal a, Eq, FloatVal b) -> BoolVal (a = b)
      | (FloatVal a, Eq, IntVal b) -> BoolVal (a = float_of_int b)
      | (IntVal a, Eq, FloatVal b) -> BoolVal ((float_of_int a) = b)
      | (BoolVal a, Eq, BoolVal b) -> BoolVal (a = b)
      | (IntVal a, Neq, IntVal b) -> BoolVal (a <> b)
      | (FloatVal a, Neq, FloatVal b) -> BoolVal (a <> b)
      | (FloatVal a, Neq, IntVal b) -> BoolVal (a <> float_of_int b)
      | (IntVal a, Neq, FloatVal b) -> BoolVal ((float_of_int a) <> b)
      | (BoolVal a, Neq, BoolVal b) -> BoolVal (a <> b)
      | (IntVal a, Lt, IntVal b) -> BoolVal (a < b)
      | (FloatVal a, Lt, FloatVal b) -> BoolVal (a < b)
      | (FloatVal a, Lt, IntVal b) -> BoolVal (a < float_of_int b)
      | (IntVal a, Lt, FloatVal b) -> BoolVal ((float_of_int a) < b)
      | (IntVal a, Gt, IntVal b) -> BoolVal (a > b)
      | (FloatVal a, Gt, FloatVal b) -> BoolVal (a > b)
      | (FloatVal a, Gt, IntVal b) -> BoolVal (a > float_of_int b)
      | (IntVal a, Gt, FloatVal b) -> BoolVal ((float_of_int a) > b)
      | (IntVal a, Lte, IntVal b) -> BoolVal (a <= b)
      | (FloatVal a, Lte, FloatVal b) -> BoolVal (a <= b)
      | (FloatVal a, Lte, IntVal b) -> BoolVal (a <= float_of_int b)
      | (IntVal a, Lte, FloatVal b) -> BoolVal ((float_of_int a) <= b)
      | (IntVal a, Gte, IntVal b) -> BoolVal (a >= b)
      | (FloatVal a, Gte, FloatVal b) -> BoolVal (a >= b)
      | (FloatVal a, Gte, IntVal b) -> BoolVal (a >= float_of_int b)
      | (IntVal a, Gte, FloatVal b) -> BoolVal ((float_of_int a) >= b)
      | (BoolVal a, And, BoolVal b) -> BoolVal (a && b)
      | (BoolVal a, Or, BoolVal b) -> BoolVal (a || b)
      
      (* Vector and matrix operations *)
      | (VectorVal v1, Plus, VectorVal v2) -> VectorVal (add_vectors v1 v2)
      
      | _ -> raise (RuntimeError "Invalid binary operation"))
  
  | UnaryOp (Not, expr) ->
      (match eval_expr ctx expr with
      | BoolVal b -> BoolVal (not b)
      | _ -> raise (RuntimeError "Not operation only applies to booleans"))
  
  | VectorLiteral v -> VectorVal v
  | MatrixLiteral m -> MatrixVal m
  
  | VectorIndex (name, idx) ->
    let vector = 
      (try Env.find name ctx.variables 
       with Not_found -> raise (RuntimeError ("Undefined vector: " ^ name))) in
    (match (vector, eval_expr ctx idx) with
    | (VectorVal v, IntVal i) -> 
        if i >= 0 && i < List.length v 
        then FloatVal (List.nth v i)
        else raise (RuntimeError "Vector index out of bounds")
    | (VectorVal _, _) -> raise (RuntimeError "Vector index must be an integer")
    | (_, _) -> raise (RuntimeError "First operand must be a vector"))
  
  | MatrixIndex (name, row, col) ->
    let matrix = 
      (try Env.find name ctx.variables 
       with Not_found -> raise (RuntimeError ("Undefined matrix: " ^ name))) in
    (match (matrix, eval_expr ctx row, eval_expr ctx col) with
    | (MatrixVal m, IntVal r, IntVal c) -> 
        if r >= 0 && r < List.length m && 
           c >= 0 && c < List.length (List.hd m)
        then FloatVal (List.nth (List.nth m r) c)
        else raise (RuntimeError "Matrix index out of bounds")
    | (MatrixVal _, _, _) -> raise (RuntimeError "Matrix indices must be integers")
    | (_, _, _) -> raise (RuntimeError "First operand must be a matrix"))


  |  MatrixRowAccess (name, idx) ->
    let matrix = 
      (try Env.find name ctx.variables 
       with Not_found -> raise (RuntimeError ("Undefined matrix: " ^ name))) in
    (match (matrix, eval_expr ctx idx) with
    | (MatrixVal m, IntVal r) -> 
        if r >= 0 && r < List.length m
        then VectorVal (List.nth m r)
        else raise (RuntimeError "Matrix row index out of bounds")
    | (MatrixVal _, _) -> raise (RuntimeError "Matrix row index must be an integer")
    | (_, _) -> raise (RuntimeError "First operand must be a matrix"))
  
  | BuiltInFunc (name, args) ->
   ( match name,args with 
    | "input",[Var f] ->
      (match f with
      "f" ->
    (  Printf.printf "Enter a float value: ";
      flush stdout;
      try
          FloatVal (float_of_string (input_line stdin))
      with Failure _ -> 
          raise (RuntimeError "Invalid float format"))
  | "i" -> ( (Printf.printf "Enter an integer value: ";
  flush stdout;
  try
      IntVal (int_of_string (input_line stdin))
  with Failure _ -> 
      raise (RuntimeError "Invalid integer format")))
  | _ -> raise (RuntimeError "Invalid input format")
          
          )
       
| "input", [Var v; IntLit dim] ->
           ( if dim <= 0 then
                raise (RuntimeError "Vector dimension must be positive")
            else
             (   Printf.printf "Enter %d space-separated values for vector:\n" dim;
                flush stdout;
                try
                    let line = input_line stdin in
                    let values = String.split_on_char ' ' line 
                                |> List.map float_of_string in
                    if List.length values <> dim then
                        raise (RuntimeError (Printf.sprintf "Expected %d values, got %d" 
                                          dim (List.length values)))
                    else
                        VectorVal values
                with Failure _ -> 
                    raise (RuntimeError "Invalid number format in vector input")))
        
| "input",[Var m; IntLit rows; IntLit cols] ->
          (  if rows <= 0 || cols <= 0 then
                raise (RuntimeError "Matrix dimensions must be positive")
            else
                (Printf.printf "Enter %d x %d matrix values row by row (%d rows, each with %d space-separated values):\n" 
                    rows cols rows cols;
                    flush stdout;
                  try
    let matrix = ref [] in
    for _ = 1 to rows do
        let line = input_line stdin in
        let row = String.split_on_char ' ' line 
                 |> List.map float_of_string in
        if List.length row <> cols then
            raise (RuntimeError (Printf.sprintf "Expected %d values in row, got %d" 
                              cols (List.length row)))
        else
            matrix := !matrix @ [row]
    done;
    MatrixVal !matrix
with Failure _ -> 
    raise (RuntimeError "Invalid number format in matrix input")))
  
  | _ ->
      
     ( let evaluated_args = List.map (eval_expr ctx) args in
      (match (name, evaluated_args) with
      | "create", [VectorVal n; FloatVal v] -> (let l = List.length n in
      if l <= 0 then
        raise (RuntimeError "Dimension can't be 0 or less")
      else
        VectorVal (create_vector l v))
      | "dim", [VectorVal v] -> IntVal (List.length v)
      | "is_zero", [VectorVal v] -> 
          BoolVal (List.for_all (fun x -> x = 0.0) v)
      | "unit", [VectorVal n] ->  (let l = List.length n in
      if l <= 0 then
        raise (RuntimeError "Dimension can't be 0 or less")
      else
        VectorVal (create_unit_vector l ))
      | "scale", [ FloatVal f ; VectorVal v] -> 
          VectorVal (List.map (fun x -> x *. f) v)
      | "addv", [VectorVal v1; VectorVal v2] -> 
          VectorVal (add_vectors v1 v2)
      | "dot_prod", [VectorVal v1; VectorVal v2] -> 
          FloatVal (dot_product v1 v2)
      | "inv", [VectorVal v] -> 
          VectorVal (List.map (fun x -> 1.0 /. x) v)
      | "length", [VectorVal v] -> 
          FloatVal (vector_length v)
      | "angle", [VectorVal v1; VectorVal v2] -> 
          FloatVal (vector_angle v1 v2)
      | "num_rows", [MatrixVal m] -> IntVal (List.length m)
      | "num_cols", [MatrixVal m] -> IntVal (List.length (List.hd m))
      | "create_matrix", [MatrixVal r; IntVal c; FloatVal v] ->
       ( let r = List.length r in
        if r > 0 && c > 0 then
          MatrixVal (List.init r (fun _ -> List.init c (fun _ -> v)))
        else
         raise (RuntimeError "Invalid matrix dimensions: Rows and columns must be positive integers."))
      | "matrix_add", [MatrixVal m1; MatrixVal m2] -> 
          MatrixVal (matrix_add m1 m2)
      | "matrix_transpose", [MatrixVal m] -> 
          MatrixVal (matrix_transpose m)
      | "matrix_determinant", [MatrixVal m] -> 
          FloatVal (matrix_determinant m)
      | "matrix_scal_mult", [MatrixVal m; FloatVal f] ->
          MatrixVal (List.map (fun row -> List.map (fun x -> x *. f) row) m)
      | "matrix_minor", [MatrixVal m; IntVal r; IntVal c] ->
          MatrixVal (matrix_minor m r c)
      | "matrix_mult", [MatrixVal m1; MatrixVal m2] ->
          MatrixVal (matrix_multiply m1 m2)
          | "update_matrix", [MatrixVal m; IntVal r; IntVal c; FloatVal v] ->
            let updated_row = 
                List.mapi (fun j x -> if j = c then v else x) (List.nth m r) in
            let updated_matrix = 
                List.mapi (fun i row -> if i = r then updated_row else row) m in
            (match args with 
            | Var name ::xs -> 
                ctx.variables <- Env.add name (MatrixVal updated_matrix) ctx.variables;
                MatrixVal updated_matrix
            | _ -> MatrixVal updated_matrix)
      | "input", [IntVal filename] ->
                ( let filepath = Printf.sprintf "%d.txt" filename in
                let read_matrix_from_file filepath =
                  let ic = open_in filepath in
                  let rows = ref [] in
                  try
                    while true do
                      let line = input_line ic in
                      let row = List.map float_of_string (String.split_on_char ' ' line) in
                      rows := !rows @ [row];  (* Append the new row to our accumulated rows *)
                    done;
                    (* This line is never reached due to the while true loop *)
                    MatrixVal !rows  
                  with
                  | End_of_file ->
                      close_in ic;
                      if !rows = [] then 
                        raise (RuntimeError "Empty matrix in file")
                      else if List.length !rows = 1 then 
                        VectorVal (List.hd !rows)  (* If only one row, return as vector *)
                      else 
                        MatrixVal !rows  (* Otherwise return as matrix *)
                  | Sys_error msg -> 
                      close_in_noerr ic;  (* Better to use noerr version in exception handler *)
                      raise (RuntimeError ("File error: " ^ msg))
                  | Failure _ -> 
                      close_in_noerr ic;
                      raise (RuntimeError "Invalid number format in file")
                in
                read_matrix_from_file filepath)
  
      | "print", [value] -> 
          (* Simplified print handling *)
          Printf.printf "%s\n" (match value with
            | IntVal i -> string_of_int i
            | FloatVal f -> string_of_float f
            | BoolVal b -> string_of_bool b
            | VectorVal v -> 
                "[" ^ String.concat "; " (List.map string_of_float v) ^ "]"
            | MatrixVal m ->
                "[" ^ String.concat "; " 
                  (List.map (fun row -> 
                    "[" ^ String.concat "; " (List.map string_of_float row) ^ "]") m) ^ "]"
            | _ -> "Unknown value");
          UnitVal
      | _ -> raise (RuntimeError ("Unknown built-in function: " ^ name)))))
  
  | ParenExpr e -> eval_expr ctx e
  
  | _ -> raise (RuntimeError "Unsupported expression")

(* Statement evaluation *)
let rec eval_stmt ctx = function
  | ExprStmt expr -> 
      let _ = eval_expr ctx expr in
      ()
  
  | IfStatement (cond, then_branch, else_branch) ->
      let result = eval_expr ctx cond in
      (match result with
      | BoolVal true -> 
          List.iter (eval_stmt ctx) then_branch
      | BoolVal false -> 
          (match else_branch with
          | Some branch -> List.iter (eval_stmt ctx) branch
          | None -> ())
      | _ -> raise (RuntimeError "Condition must be a boolean"))
  
  | WhileStatement (cond, body) ->
      let rec loop () =
        let result = eval_expr ctx cond in
        match result with
        | BoolVal true -> 
            List.iter (eval_stmt ctx) body;
            loop ()
        | BoolVal false -> ()
        | _ -> raise (RuntimeError "Condition must be a boolean")
      in
      loop ()
  
  | ForStmt (var, start, end_, body) ->
      let start_val = match eval_expr ctx start with 
        | IntVal i -> i 
        | FloatVal f -> int_of_float f 
        | _ -> raise (RuntimeError "For loop start must be a number") in
      let end_val = match eval_expr ctx end_ with
        | IntVal i -> i
        | FloatVal f -> int_of_float f
        | _ -> raise (RuntimeError "For loop end must be a number") in
      
      let rec loop i =
        if i > end_val then ()
        else begin
          ctx.variables <- Env.add var (IntVal i) ctx.variables;
          List.iter (eval_stmt ctx) body;
          loop (i + 1)
        end
      in
      loop start_val
      
  
  | RaiseError -> 
      raise (RuntimeError "Explicit error raised")
 | ReturnStmt expr -> 
    let _ = eval_expr ctx expr in
    () (* Return statements are handled elsewhere in a real implementation *)

(* Program evaluation *)
let eval_prog prog =
  try 
    List.iter (eval_stmt global_ctx) prog;
    Ok global_ctx
  with 
  | RuntimeError msg -> Error msg
  | exn -> Error (Printexc.to_string exn)