open Lexer
open Parser
open Lexing
open Ast
open Type_checker
open Eval

(* Function to convert tokens to readable strings *)
let string_of_token = function
  | VECTOR_LIT v -> Printf.sprintf "VECTOR_LIT(%s)" v
  | MATRIX_LIT m -> Printf.sprintf "MATRIX_LIT(%s)" m
  | INT_L n -> Printf.sprintf "INT_L(%d)" n
  | FLOAT_L f -> Printf.sprintf "FLOAT_L(%f)" f
  | VARIABLE v -> Printf.sprintf "VARIABLE(%s)" v
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | CREATE -> "CREATE"
  | DIMENSION -> "DIMENSION"
  | IS_ZERO -> "IS_ZERO"
  | UNIT -> "UNIT"
  | SCALE -> "SCALE"
  | ADDV -> "ADDV"
  | DOT_PROD -> "DOT_PROD"
  | INV -> "INV"
  | LENGTH -> "LENGTH"
  | ANGLE -> "ANGLE"
  | ROWSHIFT -> "ROWSHIFT"
  | ROWSUBTRACT -> "ROWSUBTRACT"
  | INPUT -> "INPUT"
  | PRINT_FILE -> "PRINT_FILE"
  | NUM_ROWS -> "NUM_ROWS"
  | NUM_COLS -> "NUM_COLS"
  | CREATE_MATRIX -> "CREATE_MATRIX"
  | MATRIX_ADD -> "MATRIX_ADD"
  | MATRIX_TRANSPOSE -> "MATRIX_TRANSPOSE"
  | MATRIX_DETERMINANT -> "MATRIX_DETERMINANT"
  | MATRIX_SCAL_MULT -> "MATRIX_SCAL_MULT"
  | MATRIX_MINOR -> "MATRIX_MINOR"
  | MATRIX_MULTIPLY -> "MATRIX_MULTIPLY"
  | UPDATE_MATRIX -> "UPDATE_MATRIX"
  | EQ -> "EQ"
  | NOTEQ -> "NOTEQ"
  | LT -> "LT"
  | GT -> "GT"
  | LTE -> "LTE"
  | GTE -> "GTE"
  | ASSIGN -> "ASSIGN"
  | SEMI -> "SEMI"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | FOR -> "FOR"
  | TO -> "TO"
  | WHILE -> "WHILE"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | MOD -> "MOD"
  | POWER -> "POWER"
  | LCURLY -> "LCURLY"
  | RCURLY -> "RCURLY"
  | LSQUARE -> "LSQUARE"
  | RSQUARE -> "RSQUARE"
  | COMMA -> "COMMA"
  | LCURVED -> "LCURVED"
  | RCURVED -> "RCURVED"
  | NOT -> "NOT"
  | OR -> "OR"
  | AND -> "AND"
  | RAISE -> "RAISE"
  | ERROR -> "ERROR"
  | EOF -> "EOF"

(* Helper function to print runtime values for debugging *)
let rec string_of_runtime_value = function
  | IntVal i -> Printf.sprintf "IntVal(%d)" i
  | FloatVal f -> Printf.sprintf "FloatVal(%f)" f
  | BoolVal b -> Printf.sprintf "BoolVal(%b)" b
  | VectorVal v -> 
      "VectorVal[" ^ 
      (String.concat ", " (List.map string_of_float v)) ^ 
      "]"
  | MatrixVal m ->
      "MatrixVal[" ^ 
      (String.concat ", " 
         (List.map (fun row -> 
            "[" ^ (String.concat ", " (List.map string_of_float row)) ^ "]") m)) ^
      "]"
  | UnitVal -> "UnitVal"

(* Helper function to print the context's variables *)
let print_context ctx =
  Printf.printf "Final Context Variables:\n";
  Env.iter (fun name value ->
    Printf.printf "  %s = %s\n" name (string_of_runtime_value value)
  ) ctx.variables

(* Function to print tokens *)
let print_tokens (lexbuf: lexbuf) =
  let rec aux () =
    let token = Lexer.tokenize lexbuf in
    match token with
    | EOF -> Printf.printf "EOF\n"; ()
    | _ -> Printf.printf "%s " (string_of_token token); aux ()
  in
  aux ()

let parse_and_display (str: string) =
  let lexbuf = Lexing.from_string str in
  
  (* Print tokens before parsing *)
  Printf.printf "\nTokens: ";
  let lexbuf_copy = Lexing.from_string str in (* Copy to avoid consuming tokens *)
  (try
    print_tokens lexbuf_copy;
    Printf.printf "\n";
  with
  | Failure msg -> Printf.printf "\nLexer token printing error: %s\n" msg
  | e -> Printf.printf "\nUnexpected error in token printing: %s\n" (Printexc.to_string e));
  
  (* Parse input and handle errors *)
  try
    Printf.printf "Attempting to parse...\n";
    let ast = Parser.prog Lexer.tokenize lexbuf in
    Printf.printf "Parsing successful! AST:\n%s\n" (pprint_prog ast);
    
    (* Type check the AST *)
    Printf.printf "Type checking:\n";
    match type_check_prog ast with
    | Ok _ -> 
        Printf.printf "Type check successful!\n";
        
        (* Reset global context before each evaluation *)
        let empty_env = Env.empty in
        global_ctx.variables <- empty_env;
        
        (* Evaluate the program *)
        Printf.printf "Evaluating program...\n";
        (match eval_prog ast with
        | Ok ctx -> 
            Printf.printf "Evaluation successful!\n";
            print_context ctx
        | Error err -> 
            Printf.printf "Runtime error: %s\n" err)
    | Error _ -> 
        Printf.printf "Type check failed!\n"
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.lex_curr_p in
      Printf.printf "Syntax error at line %d, column %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
  | Failure msg ->
      Printf.printf "Parser error: %s\n" msg
  | e ->
      Printf.printf "Unknown parsing error occurred: %s\n" (Printexc.to_string e)

let () =
  let test_cases = [
    (* Test case 1: Matrix addition with proper variable declarations *)
    "b := 1 * 2;
    Print(b);";
    
     (* Test case 2: Matrix transpose with proper variable declaration *)
    "b := [[1;2];[3;4]];
     a := matrix_transpose(b);
     Print(a);
     Print(b);";
     
    
    (* Test case 3: Matrix determinant with proper variable declaration *)
    "b := [[1;2];[3;4]];
     a := matrix_determinant(b);
     Print(a);
     Print(b);"
    ;
    (* Test case 4: Matrix multiplication with proper variable declarations *)
    "b := [[1;2];[3;4]];
     c := [[5;6];[7;8]];
     a := matrix_mult(b,c);
     Print(a);
     Print(b);
     Print(c);";
    
    (* Test case 5: Matrix cofactor calculation with proper variable declarations *)
    "a := [[1;9;3];[4;5;6];[7;8;9]];
     if(matrix_determinant(a) <> 0) then {
         cofactor_matrix := create_matrix(a,num_cols(a),0.0);
         for i := 0 to (num_rows(a) - 1) {
             for j := 0 to (num_cols(a) - 1) {
                 minor := matrix_minor(a,i,j);
                 if ((i + j)%2 = 0) then {
                    update_matrix(cofactor_matrix,i,j,matrix_determinant(minor)) ;
                 } else {
                    update_matrix(cofactor_matrix,i,j,(-1) * matrix_determinant(minor)) ;
                 }
             }
         }
         adjoint_of_matrix := matrix_transpose(cofactor_matrix);
         inverse_of_matrix := matrix_scal_mult(adjoint_of_matrix,1. / matrix_determinant(a));
     } else {
         raise error;
     }
      Print(a);
      Print(inverse_of_matrix);";
    
    (* Test case 6: Eigenvalue calculation with proper variable declarations *)
    "a := [[1;2];[3;4]];
     d := 5;
     trace := a[0,0] + a[1,1];
     determinant := matrix_determinant(a);
     d := trace * trace - 4 * determinant;
     if (d >= 0) then {
         eigenvalue1 := (trace + d ** (0.5)) / 2;
         eigenvalue2 := (trace - d**(0.5)) / 2;
     } else {
         raise error;
     }  Print(a);
        Print(eigenvalue1);
        Print(eigenvalue2);
        " 
;
        
    (*Typechecking error Error vectors of different dimensions added*)
      "
     a := [1;2];
     b := [1];
     addv(a,b);";  

    (* Typechecking Error determinant of non square matrix*)

      "
     a := [[1;2;3];[4;5;6]];
     b := matrix_determinant(a);
     " ; 

     (* Undefined variable used*)

      "
     b := a + 2;
     " ;  

     (* Not applied to a number type error*)

     "
     b := not 1;
     " ; 

     (*If condition requires a boolean*)
 
     "
     if( 1 + 2) then {
      x := 2;
    } else {
               raise error;

      }
     " ;

     (*For loop must start with a number*)

      "
     for i := k to 2 {
      x := 1;
     }" ;
(* Testing the inbuilt functionality*)
     "v1 := [1.0; 2.0; 3.0];
     v2 := [4.0; 5.0; 6.0];
     Print(v1);
     Print(v2);
     sum := addv(v1, v2);
     Print(sum);
      len := length(v1);
     Print(len);
      scaled := scale( 2.0 , v1);
     Print(scaled);
     dot := dot_prod(v1, v2);
     Print(dot);
    " ; 
(*Gaussian Elimination*)
    "a := Input(2);
     if(matrix_determinant(a) <> 0) then {
         cofactor_matrix := create_matrix(a,num_cols(a),0.0);
         for i := 0 to (num_rows(a) - 1) {
             for j := 0 to (num_cols(a) - 1) {
                 minor := matrix_minor(a,i,j);
                 if ((i + j)%2 = 0) then {
                    update_matrix(cofactor_matrix,i,j,matrix_determinant(minor)) ;
                 } else {
                    update_matrix(cofactor_matrix,i,j,(-1) * matrix_determinant(minor)) ;
                 }
             }
         }
         adjoint_of_matrix := matrix_transpose(cofactor_matrix);
         inverse_of_matrix := matrix_scal_mult(adjoint_of_matrix,1. / matrix_determinant(a));
         col_t := Input(3);
        x := matrix_mult(inverse_of_matrix,col_t);
        Print(x);
     } else {
         raise error;
     }
      ";
(*File opening check*)
      "
      a := Input(2);
      ";
    
      "
      a := 1/0;
      " ;
      (*Integer input*)
      "
      a := Input(i);
      Print(a);
      "
;
      (*Float Input*)
      "
      a := Input(f);
      Print(a);
      ";

      (*Runtime error of vector index out of bounds*)
      "
        a := [1;2];
        b := a[3,,];
      
      "


     
     






     
   
  
     
    ] in
  Printf.printf "Running %d test cases...\n\n" (List.length test_cases);
  List.iteri (fun i test_case ->
    Printf.printf "============= TEST CASE %d =============\n" (i + 1);
    Printf.printf "Code:\n%s\n" test_case;
    parse_and_display test_case;
    Printf.printf "======================================\n\n";
  ) test_cases