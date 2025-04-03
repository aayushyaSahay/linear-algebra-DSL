{
  open Parser
  exception Lexing_error of string
}

(* Helper definitions *)
let digit = ['0'-'9']
let integer = digit+  
let float = digit+ '.' digit* | '.' digit+ | digit '.' digit* ['e' 'E']['+' '-']?digit+ (* Matches float literals *)
let identifier = ['a'-'z' 'A'-'Z' '_' '\''] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let invalidident = digit+ identifier


rule segmentar = parse
  | [' ' '\t' '\r' '\n']+ { segmentar lexbuf }    (* Whitespaces *)

  (* Comments *)
  | "//" [^ '\n']* '\n' { segmentar lexbuf }
  | "/*" { comment lexbuf }
  | '"'  [^ '"']* '"' as str { TokSTRING(str) }

  (* Type Keywords *)
  | "int" { TypeINT }
  | "float" { TypeFLOAT }
  | "bool" { TypeBOOL }
  | "ivector" { TypeIVECT }
  | "fvector" { TypeFVECT }
  | "imatrix" { TypeIMAT }
  | "fmatrix" { TypeFMAT }

  (* Function Keywords *)
  | "Input" { INPUT }
  | "Print" { PRINT }
  | "abs" { ABS }
  | "dim" { DIM }
  | "magn" { MAGN }
  | "angle" { ANGLE }
  | "det" { DET }
  | "trans" { TRANS }
  | "create_ivect"  { CRT_IVECT }
  | "create_fvect"  { CRT_FVECT }
  | "create_imat"   { CRT_IMAT }
  | "create_fmat"   { CRT_FMAT }
  | "minor"         { MINOR }
  | "inverse"       { INVERT }

  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "for" { FOR }

  (* Operators and punctuation *)
  | ":=" { ASSIGN }
  | "==" { EQ }
  | "!=" { NOTEQ}
  | "<=" { LE }
  | ">=" { GE }
  | "<" { LT }
  | ">" { GT }
  | "||" { OR }
  | "&&" { AND }
  | "~" { NOT }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "**"{ DOTPROD }
  | "/" { FSLASH }
  | "%" { MODULUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "sqrt" { SQRT }
  | "exit" { EXIT }


  (* Literals *)
  | "true" { TokBOOL true }
  | "false" { TokBOOL false }
  | float as f { TokFLOAT (float_of_string f) }
  | integer as i { TokINT (int_of_string i) }

  (* Identifier *)
  | invalidident as id { raise (Lexing_error (Printf.sprintf "Invalid identifier: %s" id)) }
  | identifier as id { TokIDENT id }

  | eof { EOF }

  | _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) }

and comment = parse
  | "*/" { segmentar lexbuf }
  | eof { raise (Lexing_error "Unterminated comment") }
  | _ { comment lexbuf }
