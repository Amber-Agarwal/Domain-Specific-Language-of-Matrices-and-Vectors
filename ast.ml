open Printf

type bin_op =
  | Plus | Minus | Mult | Div | Mod
  | Eq | Neq | Lt | Gt | Lte | Gte
  | And | Or | Pow  

type unary_op = Not

type expr =
  | IntLit of int
  | FloatLit of float
  | BoolLit of bool
  | Var of string
  | Assign of expr * expr
  | BinOp of expr * bin_op * expr
  | UnaryOp of unary_op * expr
  | ParenExpr of expr
  | VectorLiteral of (float list)
  | MatrixLiteral of (float list list)
  | VectorIndex of string * expr
  | MatrixIndex of string * expr * expr
  | MatrixRowAccess of string * expr
  | BuiltInFunc of string * expr list

type statement =
  | ExprStmt of expr
  | ReturnStmt of expr
  | IfStatement of expr * statement list * statement list option
  | WhileStatement of expr * statement list
  | ForStmt of string * expr * expr * statement list
  | RaiseError

type prog = statement list






(* Helper function to safely convert strings to float, handling both integer and float formats *)
let safe_float_of_string s =
  let s = String.trim s in
  try float_of_string s
  with Failure _ -> 
    try float_of_int (int_of_string s)
    with Failure msg -> 
      failwith ("Could not convert to number: " ^ s ^ " - " ^ msg)

(* Parse a vector literal string like "[1, 2, 3]" into a list of floats *)
let parse_vector str =
  (* Remove brackets and whitespace *)
  let str = String.trim str in
  let content = String.sub str 1 (String.length str - 2) in
  
  (* Handle empty vector *)
  if String.trim content = "" then []
  else
    (* Split by commas *)
    let elements = String.split_on_char ';' content in
    (* Convert each element to float safely *)
    List.map safe_float_of_string elements

(* Parse a matrix literal string like "[[1,2],[3,4]]" into a list of lists of floats *)
let parse_matrix str =
  (* Clean the input string *)
  let str = String.trim str in
  
  (* Check if it's an empty matrix *)
  if str = "[]" then []
  else
    (* Remove the outer brackets *)
    let content = String.sub str 1 (String.length str - 2) in
    
    (* Manually parse the matrix structure *)
    let rows = ref [] in
    let current_row = ref [] in
    let current_num = ref "" in
    let in_row = ref false in
    
    (* Function to add the current number to the current row *)
    let add_number () =
      if !current_num <> "" then (
        let num = safe_float_of_string !current_num in
        current_row := num :: !current_row;
        current_num := ""
      )
    in
    
    (* Process each character in the string *)
    for i = 0 to String.length content - 1 do
      match content.[i] with
      | '[' -> 
          in_row := true;
          current_row := []
      | ']' -> 
          add_number ();
          in_row := false;
          if !current_row <> [] then
            rows := (List.rev !current_row) :: !rows
      | ';' -> 
          if !in_row then add_number ()
      | ' ' | '\t' | '\n' | '\r' -> 
          () (* Skip whitespace *)
      | c when (c >= '0' && c <= '9') || c = '.' || c = '-' || c = '+' ->
          current_num := !current_num ^ (String.make 1 c)
      | _ -> () (* Ignore other characters *)
    done;
    
    (* Return the parsed matrix *)
    List.rev !rows

    (* Pretty-print expressions *)
    let rec pprint_expr = function
      | IntLit i -> sprintf "EInt(%d)" i
      | FloatLit f -> sprintf "EFloat(%f)" f
      | BoolLit b -> sprintf "EBool(%b)" b
      | Var v -> sprintf "Var(%s)" v
      | Assign (e1, e2) -> sprintf "Assign(%s, %s)" (pprint_expr e1) (pprint_expr e2)
      | BinOp (e1, bop, e2) -> 
          let bop_str = match bop with
            | Plus -> "+" | Minus -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
            | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">" | Lte -> "<=" | Gte -> ">="
            | And -> "&&" | Or -> "||" | Pow -> "**"
          in
          sprintf "BinOp(%s %s %s)" (pprint_expr e1) bop_str (pprint_expr e2)
      | UnaryOp (op, e) ->
          let op_str = match op with | Not -> "!" in
          sprintf "UnaryOp(%s %s)" op_str (pprint_expr e)
      | ParenExpr e -> sprintf "Paren(%s)" (pprint_expr e)
      | VectorLiteral v -> 
          sprintf "Vector([%s])" (String.concat "; " (List.map string_of_float v))
      | MatrixLiteral m ->
          sprintf "Matrix([%s])" (String.concat "; " (List.map (fun row -> 
            "[" ^ (String.concat "; " (List.map string_of_float row)) ^ "]") m))
      | VectorIndex (name, idx) -> sprintf "%s[%s]" name (pprint_expr idx)
      | MatrixIndex (name, row, col) -> sprintf "%s[%s, %s]" name (pprint_expr row) (pprint_expr col)
      | MatrixRowAccess (name,idx) -> sprintf "%s[%s]" name (pprint_expr idx)
      | BuiltInFunc (name, args) -> sprintf "BuiltInFunc(%s, [%s])" name (String.concat ", " (List.map pprint_expr args))
    
    (* Pretty-print statements *)
    let rec pprint_stmt = function
      | ExprStmt e -> sprintf "ExprStmt(%s)" (pprint_expr e)
      | ReturnStmt e -> sprintf "Return(%s)" (pprint_expr e)
      | IfStatement (cond, then_branch, else_branch) ->
          let then_str = String.concat " " (List.map pprint_stmt then_branch) in
          let else_str = match else_branch with
            | Some stmts -> " else { " ^ (String.concat " " (List.map pprint_stmt stmts)) ^ " }"
            | None -> ""
          in
          sprintf "If(%s) { %s }%s" (pprint_expr cond) then_str else_str
      | WhileStatement (cond, body) ->
          sprintf "While(%s) { %s }" (pprint_expr cond) (String.concat " " (List.map pprint_stmt body))
      | ForStmt (var, start, end_, body) ->
          sprintf "For(%s = %s to %s) { %s }" var (pprint_expr start) (pprint_expr end_) (String.concat " " (List.map pprint_stmt body))
      | RaiseError -> "RaiseError"
      (* | FuncDef (name, args, body) ->
          let args_str = String.concat ", " (List.map pprint_expr args) in
          sprintf "FuncDef(%s, [%s]) { %s }" name args_str (String.concat " " (List.map pprint_stmt body)) *)
    
    (* Pretty-print the entire program *)
    let pprint_prog (prog: prog) =
      String.concat "\n" (List.map pprint_stmt prog)
