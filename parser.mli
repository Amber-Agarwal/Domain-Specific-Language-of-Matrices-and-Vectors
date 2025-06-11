type token =
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | WHILE
  | FOR
  | TO
  | POWER
  | PLUS
  | MINUS
  | MULT
  | DIV
  | MOD
  | EQ
  | NOTEQ
  | LT
  | GT
  | LTE
  | GTE
  | AND
  | OR
  | NOT
  | ASSIGN
  | LCURVED
  | RCURVED
  | LSQUARE
  | RSQUARE
  | LCURLY
  | RCURLY
  | COMMA
  | SEMI
  | INT_L of (int)
  | FLOAT_L of (float)
  | VARIABLE of (string)
  | VECTOR_LIT of (string)
  | MATRIX_LIT of (string)
  | RAISE
  | ERROR
  | EOF
  | CREATE
  | DIMENSION
  | IS_ZERO
  | UNIT
  | SCALE
  | ADDV
  | DOT_PROD
  | INV
  | LENGTH
  | ANGLE
  | ROWSHIFT
  | ROWSUBTRACT
  | NUM_ROWS
  | NUM_COLS
  | CREATE_MATRIX
  | MATRIX_ADD
  | MATRIX_TRANSPOSE
  | MATRIX_DETERMINANT
  | MATRIX_SCAL_MULT
  | MATRIX_MINOR
  | MATRIX_MULTIPLY
  | UPDATE_MATRIX
  | INPUT
  | PRINT_FILE

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
