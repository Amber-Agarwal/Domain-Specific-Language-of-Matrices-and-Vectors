{
    open Parser
}

let digits = ['0'-'9']
let small_letters = ['a'-'z']
let capital_letters = ['A'-'Z']
let integer = ['+' '-']?['0'-'9']+
let float = ['+' '-']?(['0'-'9']+'.'['0'-'9']*|'.'['0'-'9']+)
let booleans = ('T'|'F')
let var = (small_letters) (small_letters | capital_letters | digits | ['_' '.' '-' '$'])*  
let whitespace = [' ' '\t' '\r' '\n']+
let vector_lit = "[" whitespace* ( (digits|float) (whitespace* ";" whitespace* (digits|float) )*)? whitespace* "]"
let matrix_lit = "[" whitespace* (vector_lit (whitespace* ";" whitespace* vector_lit)*)? whitespace* "]"

rule tokenize = parse
| whitespace { tokenize lexbuf } (* Fix: Consuming all whitespace correctly *)
| "(*" { comment 0 lexbuf; tokenize lexbuf }
| "T" {TRUE}    
| "F" {FALSE} 
| "create" {CREATE}
| "dim" {DIMENSION}
| "is_zero" {IS_ZERO}
| "unit" {UNIT}
| "scale" {SCALE}
| "addv" {ADDV}
| "dot_prod" {DOT_PROD}
| "inv" {INV}
| "length" {LENGTH}
| "angle" {ANGLE}
| "row_shift" {ROWSHIFT}
| "row_sub" {ROWSUBTRACT}
| "Input"  {INPUT}
| "Print"  { PRINT_FILE }
| "=" {EQ} 
| ":=" { ASSIGN } 
| ";" { SEMI }
| "if" { IF } 
| "then" { THEN }
| "else" { ELSE } 
| "for" { FOR } 
| "to" { TO } 
| "while" { WHILE } 
| "+" { PLUS } 
| "-" { MINUS } 
| "*" { MULT } 
| "/" { DIV } 
| "%" { MOD } 
| "<" { LT } 
| ">" { GT } 
| "<=" { LTE } 
| "**" {POWER}
| ">=" { GTE } 
| "{" { LCURLY } 
| "}" { RCURLY } 
| "[" { LSQUARE } 
| "]" { RSQUARE } 
| "," { COMMA } 
| "(" { LCURVED } 
| ")" { RCURVED } 
| "<>" { NOTEQ } 
| "num_rows" { NUM_ROWS }
| "num_cols" { NUM_COLS }
| "create_matrix" { CREATE_MATRIX }
| "matrix_add" {MATRIX_ADD}
| "matrix_transpose" {MATRIX_TRANSPOSE}
| "matrix_determinant" {MATRIX_DETERMINANT}
| "matrix_scal_mult" {MATRIX_SCAL_MULT}
| "matrix_minor" {MATRIX_MINOR} 
| "matrix_mult" {MATRIX_MULTIPLY}
| "update_matrix" {UPDATE_MATRIX}
| "not" { NOT } 
| "or" { OR } 
| "and" {AND}
| "raise" {RAISE}
| "error" {ERROR} 
| vector_lit as v { VECTOR_LIT v }
| matrix_lit as m { MATRIX_LIT m }
| integer as i { INT_L (int_of_string i) }
| float as f { FLOAT_L (float_of_string f) }

| var as variable {VARIABLE variable}
| eof { EOF }

(* Catch-all rule to report unknown characters instead of failing silently *)
| _ as c { failwith (Printf.sprintf "Lexer error: unexpected character '%c'" c) }

and comment depth = parse
| "*)" { if depth = 0 then () else comment (depth - 1) lexbuf }
| "(*" { comment (depth + 1) lexbuf; comment depth lexbuf }
| _ { comment depth lexbuf }