%{
  open Ast  

%}

%token TRUE FALSE
%token IF THEN ELSE WHILE FOR TO
%token POWER 
%token PLUS MINUS MULT DIV MOD
%token EQ NOTEQ LT GT LTE GTE AND OR NOT
%token ASSIGN
%token  LCURVED RCURVED LSQUARE RSQUARE LCURLY RCURLY
%token COMMA SEMI
%token <int> INT_L
%token <float> FLOAT_L
%token <string> VARIABLE
%token <string> VECTOR_LIT
%token <string> MATRIX_LIT
%token RAISE ERROR
%token EOF

%token CREATE DIMENSION IS_ZERO UNIT SCALE ADDV DOT_PROD INV LENGTH ANGLE ROWSHIFT ROWSUBTRACT NUM_ROWS  NUM_COLS CREATE_MATRIX MATRIX_ADD MATRIX_TRANSPOSE MATRIX_DETERMINANT MATRIX_SCAL_MULT MATRIX_MINOR MATRIX_MULTIPLY UPDATE_MATRIX

%token INPUT PRINT_FILE

%start prog
%type <Ast.prog> prog

%right ASSIGN
%left OR
%left AND
%left EQ NOTEQ LT GT LTE GTE
%left PLUS MINUS
%left MULT DIV MOD
%right NOT

%nonassoc THEN
%nonassoc ELSE

%%

prog:
  | statement_list EOF { $1 }

statement_list:
  | statement statement_list { $1 :: $2 }
  | statement { [$1] }

statement:
  | expr SEMI { ExprStmt $1 }
  | IF LCURVED expr RCURVED THEN LCURLY statement_list RCURLY ELSE LCURLY statement_list RCURLY
      { IfStatement ($3, $7, Some $11) } 
  | WHILE LCURVED expr RCURVED LCURLY statement_list RCURLY
      { WhileStatement ($3, $6) }
  | FOR VARIABLE ASSIGN expr TO expr LCURLY statement_list RCURLY
      { ForStmt ($2, $4, $6, $8) } 
  | RAISE ERROR SEMI { RaiseError } 
  

expr:
  | logical_expr { $1 }
  | expr ASSIGN expr { Assign ($1, $3) }

logical_expr:
  | comparison_expr { $1 }
  | logical_expr AND logical_expr { BinOp ($1, And, $3) }
  | logical_expr OR logical_expr { BinOp ($1, Or, $3) }
  | NOT logical_expr { UnaryOp (Not, $2) }

comparison_expr:
  | addition_expr { $1 }
  | addition_expr EQ addition_expr { BinOp ($1, Eq, $3) }
  | addition_expr NOTEQ addition_expr { BinOp ($1, Neq, $3) }
  | addition_expr LT addition_expr { BinOp ($1, Lt, $3) }
  | addition_expr GT addition_expr { BinOp ($1, Gt, $3) }
  | addition_expr LTE addition_expr { BinOp ($1, Lte, $3) }
  | addition_expr GTE addition_expr { BinOp ($1, Gte, $3) }

addition_expr:
  | multiplication_expr { $1 }
  | addition_expr PLUS multiplication_expr { BinOp ($1, Plus, $3) }
  | addition_expr MINUS multiplication_expr { BinOp ($1, Minus, $3) }

multiplication_expr:
  | power_expr { $1 }
  | multiplication_expr MULT multiplication_expr { BinOp ($1, Mult, $3) }
  | multiplication_expr DIV multiplication_expr { BinOp ($1, Div, $3) }
  | multiplication_expr MOD multiplication_expr { BinOp ($1, Mod, $3) }

power_expr:
  | factor { $1 }
  | factor POWER power_expr { BinOp ($1, Pow, $3) }
// unary_expr:
//   | factor { $1 }
//   | MINUS unary_expr %prec NOT { UnaryOp (Neg, $2) }

factor:
  | VARIABLE { Var $1 }
  | INT_L { IntLit $1 }
  | FLOAT_L { FloatLit $1 }
  | TRUE { BoolLit true }
  | FALSE { BoolLit false }
  | LCURVED expr RCURVED { ParenExpr $2 }
  | VECTOR_LIT { VectorLiteral (parse_vector $1) }
  | MATRIX_LIT { MatrixLiteral (parse_matrix $1) }
  | VARIABLE LSQUARE expr COMMA SEMI RSQUARE { MatrixRowAccess ($1, $3) }
  | VARIABLE LSQUARE expr COMMA COMMA RSQUARE  { VectorIndex ($1, $3) }
  | VARIABLE LSQUARE expr COMMA expr RSQUARE { MatrixIndex ($1, $3, $5) }
  | function_call { $1 }

function_call:
  | built_in_function { $1 }

built_in_function:
  | CREATE LCURVED expr COMMA expr RCURVED { BuiltInFunc ("create", [$3; $5]) }
  | DIMENSION LCURVED expr RCURVED { BuiltInFunc ("dim", [$3]) }
  | IS_ZERO LCURVED expr RCURVED { BuiltInFunc ("is_zero", [$3]) }
  | UNIT LCURVED expr RCURVED { BuiltInFunc ("unit", [$3]) }
  | SCALE LCURVED expr COMMA expr RCURVED { BuiltInFunc ("scale", [$3; $5]) }
  | ADDV LCURVED expr COMMA expr RCURVED { BuiltInFunc ("addv", [$3; $5]) }
   | NUM_ROWS LCURVED expr RCURVED { BuiltInFunc ("num_rows", [$3]) } 
  | NUM_COLS LCURVED expr RCURVED { BuiltInFunc ("num_cols", [$3]) }  
  | CREATE_MATRIX LCURVED expr COMMA expr COMMA expr RCURVED { BuiltInFunc ("create_matrix", [$3; $5; $7]) }  
  | DOT_PROD LCURVED expr COMMA expr RCURVED { BuiltInFunc ("dot_prod", [$3; $5]) }
  | INV LCURVED expr RCURVED { BuiltInFunc ("inv", [$3]) }
  | LENGTH LCURVED expr RCURVED { BuiltInFunc ("length", [$3]) }
  | ANGLE LCURVED expr COMMA expr RCURVED { BuiltInFunc ("angle", [$3; $5]) }
  | ROWSHIFT LCURVED expr COMMA expr RCURVED { BuiltInFunc ("row_shift", [$3; $5]) }
  | ROWSUBTRACT LCURVED expr COMMA expr RCURVED { BuiltInFunc ("row_sub", [$3; $5]) }
  | MATRIX_ADD LCURVED expr COMMA expr RCURVED { BuiltInFunc ("matrix_add", [$3; $5]) }
  | MATRIX_TRANSPOSE LCURVED expr RCURVED { BuiltInFunc ("matrix_transpose", [$3]) }
  | MATRIX_DETERMINANT LCURVED expr RCURVED { BuiltInFunc ("matrix_determinant", [$3]) }
  | MATRIX_SCAL_MULT LCURVED expr COMMA expr RCURVED { BuiltInFunc ("matrix_scal_mult", [$3; $5]) }
  | MATRIX_MINOR LCURVED expr COMMA expr COMMA expr RCURVED { BuiltInFunc ("matrix_minor", [$3; $5; $7]) }
  | MATRIX_MULTIPLY LCURVED expr COMMA expr RCURVED { BuiltInFunc ("matrix_mult", [$3; $5]) }
  | UPDATE_MATRIX LCURVED expr COMMA expr COMMA expr COMMA expr RCURVED {BuiltInFunc ("update_matrix",[$3;$5;$7;$9])}
  | INPUT LCURVED opt_filename RCURVED { BuiltInFunc ("input", $3) }

  | PRINT_FILE LCURVED expr RCURVED { BuiltInFunc ("print", [$3]) }

// arg_list:
//   | expr { [$1] }
//   | expr COMMA arg_list { $1 :: $3 }

// param_list:
//   | VARIABLE { [$1] }
//   | VARIABLE COMMA param_list { $1 :: $3 }

opt_filename:
  | expr { [$1] }
  | /* empty */ { [] }
