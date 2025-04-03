%{
    open Ast
%}

%token <int> TokINT
%token <float> TokFLOAT
%token <bool> TokBOOL
%token <string> TokIDENT
%token <string> TokSTRING

%token TypeINT TypeFLOAT TypeBOOL
%token TypeIVECT TypeFVECT TypeIMAT TypeFMAT

%token ASSIGN EQ NOTEQ LE GE LT GT OR AND NOT             
%token PLUS MINUS UMINUS MULT FSLASH DOTPROD MODULUS SQRT
%token CRT_IVECT CRT_FVECT CRT_IMAT CRT_FMAT
%token ABS DIM MAGN ANGLE DET TRANS MINOR INVERT

%token IF THEN ELSE WHILE FOR
%token INPUT PRINT EXIT

%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token SEMICOLON COMMA

%token EOF

%nonassoc ASSIGN
%left OR
%left AND
%nonassoc EQ NOTEQ
%nonassoc LE GE LT GT 
%left PLUS MINUS
%left MULT FSLASH MODULUS 
%left DOTPROD
%right NOT
%nonassoc UMINUS

%start main exp
%type <Ast.expr> main
%type <Ast.expr> exp
%%

main:
    stmt_list EOF                                  { $1 }

scope:
    LBRACE stmt_list RBRACE                   { Scope($2) }
;


stmt_list:
    | /* empty */               { NoOp }
    | stmt stmt_list            { Seq($1, $2) }
    | scope stmt_list           { Seq($1, $2) }
;

stmt:
    | declaration SEMICOLON     { $1 }
    | assignment SEMICOLON      { $1 }
    | if_stmt                   { $1 }
    | while_stmt                { $1 }
    | for_stmt                  { $1 }
    | io_stmt SEMICOLON         { $1 }
/*  | exp SEMICOLON             { $1 }  Shouldn't Allow expressions as statements */
;

// let firstint := Input();
// Input(filename); - tthis is also provided but i don't know why
declaration:
    | TypeBOOL TokIDENT ASSIGN exp    { ConstructIdent($2, Bool, $4) }
    | TypeINT TokIDENT ASSIGN exp     { ConstructIdent($2, Integer, $4) }
    | TypeFLOAT TokIDENT ASSIGN exp   { ConstructIdent($2, Float, $4) }
    | TypeIVECT TokIDENT ASSIGN exp    { ConstructIdent($2, IVector, $4) }
    | TypeFVECT TokIDENT ASSIGN exp    { ConstructIdent($2, FVector, $4) }
    | TypeIMAT TokIDENT ASSIGN exp     { ConstructIdent($2, IMatrix, $4) }
    | TypeFMAT TokIDENT ASSIGN exp     { ConstructIdent($2, FMatrix, $4) }
;

assignment:
    | TokIDENT ASSIGN exp            { AssignIdent($1, $3) }
    | TokIDENT ASSIGN input          { AssignIdent($1, $3) }
    | TokIDENT LBRACKET exp RBRACKET ASSIGN exp   { AssignVectorAtIndex($1,$3,$6) }
    | TokIDENT LBRACKET exp RBRACKET ASSIGN input { AssignVectorAtIndex($1,$3,$6) }
    | TokIDENT LBRACKET exp RBRACKET LBRACKET exp RBRACKET ASSIGN exp   { AssignMatAtIndex($1,$3,$6,$9) }
    | TokIDENT LBRACKET exp RBRACKET LBRACKET exp RBRACKET ASSIGN input { AssignMatAtIndex($1,$3,$6,$9) }
//  either constant indexing or variable indexing, nothing other than that allowed.
;

// if(exp)then{stmt_list}else{stmt_list} // *SCOPES ARE OPTIONAL:stmt_list can be empty 
if_stmt:
    | IF LPAREN exp RPAREN THEN scope ELSE scope 
      { IfThenElse($3, $6, $8) }
;


// while(exp){stmt_list}
while_stmt:
    | WHILE LPAREN exp RPAREN scope
      { WhileLoop($3, $5) }
;

// TODO: pahle block optional karna hai?
// for(declaration; exp; stmt_list){stmt_list}
for_stmt:
    | FOR LPAREN declaration SEMICOLON exp SEMICOLON stmt_list RPAREN scope
      { ForLoop($3, $5, $7, $9) }
    | FOR LPAREN assignment SEMICOLON exp SEMICOLON stmt_list RPAREN scope
      { ForLoop($3, $5, $7, $9) }
;

io_stmt:
    | input                                     { $1 }
    | output                                    { $1 }
    | EXIT                                      { Exit }
;

input:
    | INPUT LPAREN TokSTRING RPAREN             { Inp(ConstS($3))   }
    | INPUT LPAREN RPAREN                       { Inp(NoOp) }
;

output:
    | PRINT LPAREN TokIDENT RPAREN              { Out(LookupIdent($3)) }
    | PRINT LPAREN TokSTRING RPAREN             { Out(ConstS($3)) }
;
exp:
    | TokBOOL                      { ConstB $1 }
    | TokINT                       { ConstI $1 }
    | TokFLOAT                     { ConstF $1 }
    | TokSTRING                    { ConstS $1 }
    | TokIDENT                     { LookupIdent $1 }
    | LPAREN exp RPAREN            { $2 }
    | TokINT ivector_literal        { ConstIV($1,$2) }
    | TokINT fvector_literal        { ConstFV($1,$2) }
    | TokINT TokINT imatrix_literal { ConstIM($1,$2,$3) }
    | TokINT TokINT fmatrix_literal { ConstFM($1,$2,$3) }
    | binary_op                    { $1 }
    | unary_op                     { $1 }
    | func_op                      { $1 }
    | rel_op                       { $1 }
    | TokIDENT LBRACKET exp RBRACKET { LookVectAtIndex($1,$3) }
    | TokIDENT LBRACKET exp RBRACKET LBRACKET exp RBRACKET { LookMatAtIndex($1,$3,$6) }
;

// this returns type as well as list of elements
ivector_literal:
    | LBRACKET vect_elements_int RBRACKET { $2 }
;
vect_elements_int:
    | TokINT COMMA vect_elements_int { $1 :: $3 }
    | MINUS TokINT COMMA vect_elements_int %prec UMINUS { (-$2) :: $4 }
    | TokINT                          { [$1] }
    | MINUS TokINT  %prec UMINUS      { [-$2] }
;
fvector_literal:
    | LBRACKET vect_elements_float RBRACKET { $2 }
;
vect_elements_float:
    | TokFLOAT COMMA vect_elements_float { $1 :: $3 }
    | MINUS TokFLOAT COMMA vect_elements_float %prec UMINUS { (-.$2 :: $4) }
    | TokFLOAT                          { [$1] }
    | MINUS TokFLOAT     %prec UMINUS   { [-.$2] }
;

imatrix_literal:
    | LBRACKET imat_rows RBRACKET { $2 }
;
imat_rows:
    | ivector_literal COMMA imat_rows { $1 :: $3 }
    | ivector_literal { [$1] }
;
fmatrix_literal:
    | LBRACKET fmat_rows RBRACKET { $2 }
;
fmat_rows:
    | fvector_literal COMMA fmat_rows { $1 :: $3 }
    | fvector_literal { [$1] }
;


binary_op:
    | exp PLUS exp              { Add($1, $3) }
    | exp MINUS exp             { Sub($1, $3) }
    | exp MULT exp              { ScalProd($1, $3) }
    | exp FSLASH exp            { Div($1, $3) }
    | exp MODULUS exp           { Modulo($1, $3) }
    | exp OR exp                { Add($1, $3) }
    | exp AND exp               { ScalProd($1, $3) }
    | exp DOTPROD exp           { DotProd($1, $3) }
;

unary_op:
    | NOT exp                   { Inv($2) }
    | MINUS exp %prec UMINUS    { Inv($2) }
;

func_op:
    | ABS LPAREN exp RPAREN     { Abs($3) }
    | DIM LPAREN exp RPAREN     { Dim($3) }
    | MAGN LPAREN exp RPAREN    { Abs($3) }
    | ANGLE LPAREN exp COMMA exp RPAREN { Angle($3, $5) }
    | DET LPAREN exp RPAREN     { Det($3) }
    | TRANS LPAREN exp RPAREN   { Trans($3) }
    | SQRT LPAREN exp RPAREN    { Sqrt($3) }
    | MINOR LPAREN TokIDENT COMMA exp COMMA exp RPAREN   { Minor($3,$5,$7) }
    | INVERT LPAREN exp RPAREN  { Invert($3) }
    | CRT_IVECT LPAREN TokINT RPAREN   { CIvect($3) }
    | CRT_FVECT LPAREN TokINT RPAREN   { CFvect($3) }
    | CRT_IMAT LPAREN TokINT COMMA TokINT RPAREN   { CImatr($3,$5) }
    | CRT_FMAT LPAREN TokINT COMMA TokINT RPAREN   { CFmatr($3,$5) }
;

rel_op:
    | exp EQ exp                { IsEq($1, $3) }
    | exp NOTEQ exp             { IsNotEq($1,$3) }
    | exp LE exp                { IsLe($1, $3) }
    | exp LT exp                { IsLt($1, $3) }
    | exp GE exp                { IsGe($1, $3) }
    | exp GT exp                { IsGt($1, $3) }
;
