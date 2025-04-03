### data_types:
    - int 
    - bool
    - float
    - vector:
        - ivector, fvector
    - matrix
        - imatrix, fmatrix 
        - row-major form 
        - vector of vectors

### functions to include: ( TODO: arguments may be identifiers -> implementation to be done in parser )
    input:
        - from stdin `Input()`
        - from file `Input(filename or identifier)`
    output:
        - what is its argument? (PIAZZA doubt)
        - to stdout `Print()`
        - to file   `Print(filepath or identifier)`
    integer:
        - addition  `+`
        - subtraction   `-`              -> unary operator? 
        - multiplication `*`
        - division  `/`
        - abs   `abs()`
        - equality `==`
        - inequality `!=`
        - comparisions:
            - less than `<`
            - less than equal to    `<=`
            - greater than  `>`
            - greater than equal to `>=`
        - modulo    `%`
    float:                              (no modulo with floats)
        - addition  `+`
        - subtraction   `-`
        - multiplication `*`
        - division  `/`
        - sqrt      `sqrt`
        - abs   `abs()`
        - myadditions: (1e-6)
            - equality `==`
            - inequality `!=`
            - less than `<`
            - less than equal to    `<=`
            - greater than  `>`
            - greater than equal to `>=`
    bool:
        - negation  `~`             (highest)
        - conjunction   `||`        (least )
        - disjunction   `&&`        (more precedence)
    vector:                            ( no comparisions between vectors except equality & inequality)
        - elements can only be constants values while defining/assigning a constant vector
        - create_ivect(int)              ( no ident )
        - dimension `dim()`
        - magnitude `magn( )`          (Abs(e) in AST) 
        - addition  `+`
        - scalar multiplication `*`  (int with intVectors and float with floatVectors)
        - dot product   `**`         (binds tighter than scalar product)
        - pre-multiplication by a matrix `**`
        - angle `angle( )`
        - indexing using var and constants v[i]
        - ETC
    matrix:                            ( no comparisions between vectors except equality & inequality)
        - elements can only be constants values while defining/assigning a constant matrix
        - create_fmat(int,int)         no ident allowed
        - addition  `+`
        - scalar product    `*`
        - matrix multiplication `**`
        - post multiplication by a vector `**`
        - transpose `trans( )`
        - determinant of square matrices `det( )`  NOTE: shouldn't use abs or magn for this
        - indexing using vasr and constants m[i][j]
        - minor                 `minor(mat, id/const, id/const)
        - inverse           `inverse(mat)`
        - ETC
    if_then_else:
        - three blocks within them contained within `{ }`
        - if empty within block => no command
    for loop:
        - iterator variable
        - start and end limits as variables or constants
        - body
    while loop:
        - boolean condition as an argument
        - body
    commenting:
        - mutliline and single line both same as C


### statements:
    - will be of C-type(specify data_types)
    - how to write constants? 
        - just value, i.e. <value>
    - how to write vectors?
        - ivector <varname> = <dimension><elements>
    - how to write matrices?
        - imatrix <varname> := <rows> <cols> <elements>
    - how to write variables? (RHS is a const data structure)
        - <data-type> <var_name> := <value>
    - termination of statements?
        - by ';'
    - separation of elements of vectors and matrices?
        - by ',';
    - elements contained within '[ ]'
    - how to write if_then_else?
        - if_then_else {condition} {if_true_commands} {else_commands}
    - how to write for loops?
        - for { initialization } { condition } { increment } { body }
    - how to write while loops?
        - for {condition} {body}
    - comments:
        - // for single line commenting, must have a newline at the end to avoid parse error
        - /* */ for multi-line commenting
        - nesting is not allowed 
        - but can include single line comments inside multiline comments

### test-statements:
```
    int firstint := Input();  // this needs attention
    Print(firstint);

    int secint := -4;             // minus for operators handled in parser

    firstint := firstint + secint;
    firstint := 9 - 5;
    firstint := 9 * secint;
    firstint := firstint / 5;
    firstint := (first + secint) - 7 * 0 / firstint;
    secint := abs(secint);
    firstint := firstint % secint;

    float twopointtwo := 2.2;
    float onepointone := 1.1;
    twopointtwo := twopointtwo + onepointone;
    twopointtwo := 9.1 - 5.1;
    twopointtwo := 9.2 * onepointone;
    twopointtwo := twopointtwo / 5.3;
    twopointtwo := (first + onepointone) - 7.6 * 0.0 / twopointtwo;
    twopointtwo := abs(onepointone);


    bool T := true;
    bool F := false;
    bool aretheyeq := firstint == secint;
    bool isgreater := firstint > secint;
    bool isgreatereq := firstint >= secint;
    bool islesser := firstint < secint;
    bool islessereq := firstint <= secint;

    aretheyeq := twopointtwo == onepointone;
    isgreater := twopointtwo > onepointone;
    isgreatereq := twopointtwo >= onepointone;
    islesser := twopointtwo < onepointone;
    islessereq := twopointtwo <= onepointone;


    bool aretheynoteq := ~(aretheyeq);
    bool fallacy := aretheyeq || aretheynoteq;
    bool tatutology := aretheyeq && aretheynoteq;

    ivector intv := 2 [1, 2];
    fvector x := 2 [1.0,0.0];
    fvector y := 2 [0.0,1.0];
    fvector diagv := x + y;
    fvector nullv := x + (-1 * x);
    fvector area := x ** diagv;
    int dimx := dim(diag);
    float magx := magn(diag);
    float ang1 := angle(diag, x);


    fmatrix alternatetrans := trans(transform);
    fmatrix t3 := transform + alternatetrans;
    t3 := transform ** alternatetrans;
    float det1 := det(transform);

    fvector lhs := x + y;
    lhs := x * transform;
    x := x * transform;
    y := y * transform;
    fvector rhs := x + y;


    /*
    NOTE: there should not be any semicolons in the condition blocks of if_then_else and for,while loops.
    */

    bool mycond := (lhs ** rhs) == 1;

    if_then_else (condition) {Print(TRUE);} { Print(FALSE);}
    int counter := 0;
    while (counter < 5) {Print(counter); counter := counter + 1;}

    for {int i := 0;} {i < 10} {i := i + 1;} {Print(i);}


```

### Scopes and symbol tables
    - THE PROGRAM MAY OR MAY NOT START WITH A SCOPING BRACES.
    - I will by default push a default GLOBAL STACK as i start type checking
    - { } are used to define scopes 
    - scopes are pushed onto a stack
    - these will be used by for and while loops and also if_then_else only to declare the body
    - one can separately declare them as well.
    
    
    - only one declaration of a variable is allowed in the current stack frame
    - thus contruction will search only in the current stack frame
    - else search of a variables happens recursively from the top of the stack
    - thus lookup will find the variable declared nearest to the current stack frame
    - which also means that it won't keep searching back for a variable that has the given type. rather it will search only by name and if types mismatch on the first id then it will raise error.
    - variables get destroyed out of their scopes 
    - while mutating variables, the stored values will get changed but the type remains the same
    - mutation relies on lookup of the variable at the first place


### Pseudocode and Code for Finding the determinant of a matrix and the inverse of a matrix

```JS
function determinant(matrix):
    if matrix is 1x1:
        return matrix[0][0]
    else if matrix is 2x2:
        return (matrix[0][0] * matrix[1][1]) - (matrix[0][1] * matrix[1][0])
    else:
        det = 0
        for col in 0 to length(matrix[0]) - 1:
            submatrix = getSubmatrix(matrix, 0, col)
            det += ((-1) ^ col) * matrix[0][col] * determinant(submatrix)
        return det

function getSubmatrix(matrix, row, col):
    submatrix = []
    for i in 0 to length(matrix) - 1:
        if i == row:
            continue
        submatrixRow = []
        for j in 0 to length(matrix[0]) - 1:
            if j == col:
                continue
            submatrixRow.append(matrix[i][j])
        submatrix.append(submatrixRow)
    return submatrix
```
```OCaml
    determinant wala to OCaml se hi ho jayega implement
```

```JS
function inverse(matrix):
    det = determinant(matrix)
    if det == 0:
        return "Matrix is not invertible"
    
    adjugateMatrix = adjugate(matrix)
    inverseMatrix = []
    for i in 0 to length(adjugateMatrix) - 1:
        inverseRow = []
        for j in 0 to length(adjugateMatrix[0]) - 1:
            inverseRow.append(adjugateMatrix[i][j] / det)
        inverseMatrix.append(inverseRow)
    return inverseMatrix

function adjugate(matrix):
    adjugateMatrix = []
    for i in 0 to length(matrix) - 1:
        adjugateRow = []
        for j in 0 to length(matrix[0]) - 1:
            submatrix = getSubmatrix(matrix, i, j)
            cofactor = ((-1) ^ (i + j)) * determinant(submatrix)
            adjugateRow.append(cofactor)
        adjugateMatrix.append(adjugateRow)
    return transpose(adjugateMatrix)

function transpose(matrix):
    transposedMatrix = []
    for i in 0 to length(matrix[0]) - 1:
        transposedRow = []
        for j in 0 to length(matrix) - 1:
            transposedRow.append(matrix[j][i])
        transposedMatrix.append(transposedRow)
    return transposedMatrix
```

```mylang

fmatrix m1 := 2 2 [[1.0,2.0],[3.0,4.0]];

inverse(matrix):
    float d1 := det(matrix);
    if(d1 = 0) then { do nothing } else{
        fmatrix adjugateMatrix := 2 2 [[0.0,0.0],[0.0,0.0]];

        for(int i = 0; i < row - 1; i := i + 1;){
            for(int j = 0; j < col - 1; j := j + 1;){
                
                submatrix = shape []
                for (int ii := 0; ii < row-1;):
                    if ii == row:
                        continue
                    submatrixRow = []
                    for jj in 0 to length(matrix[0]) - 1:
                        if jj == col:
                            continue
                        submatrixRow.append(matrix[ii][jj])
                    submatrix.append(submatrixRow)
                
                float cofactor := -1.0;
                if((i+j)%2 == 0) then{
                    cofactor := -1.0;
                }else{
                    cofactor := 1.0;
                }
                cofactor := cofactor * det(submatrix);


            }
        }

    }


submatrix = []
for i in 0 to length(matrix) - 1:
    if i == row:
        continue
    submatrixRow = []
    for j in 0 to length(matrix[0]) - 1:
        if j == col:
            continue
        submatrixRow.append(matrix[i][j])
    submatrix.append(submatrixRow)

```



i) SAMAJH HI NAHI AAYA, DIMAG NAHI LAGANA CHAHTA HU
# Control Flow - For Loop (for n x 3 matrix - where n is a fixed integer, say 5)
import Matrix
vector_sum = [0, 0, 0]
for i = 0 to n-1:
    vector_sum := Matrix.add(vector_sum, V[i])


# DONE Matrix Sum
import Matrix
add_matrices = Matrix.add(A, B)

(this is just a functionality, ideally you would declare matrices A, B, read them, declare a matrix add_matrices, and emit/print its contents.)

# DONE Matrix Transpose
import Matrix
transpose_matrix = Matrix.transpose(A)

# DONE Matrix Determinant
import Matrix
determinant_of_matrix = Matrix.determinant(A)

# DONE Matrix Inverse and Control Flow - If-Then-Else
import Matrix
if Matrix.determinant(A) != 0:
    cofactor_matrix = Matrix.create_empty(A.rows, A.columns)
    for i = 0 to A.rows-1:
        for j = 0 to A.columns-1:
            minor = Matrix.minor(A, i, j)
            cofactor_matrix[i, j] := (-1)^(i+j) * Matrix.determinant(minor)
    adjoint_of_matrix = Matrix.transpose(cofactor_matrix)
    inverse_of_matrix = Matrix.scalar_multiply(1 / Matrix.determinant(A), adjoint_of_matrix)
else:
    raise MatrixNotInvertible


# DONE Matrix Multiplication
import Matrix
multiply_matrices = Matrix.multiply(A, B)

# DONE Matrix and Vector Product
import Matrix
multiply_vector_matrix = Matrix.vector_multiply(A, x)
(here the multiply_vector_matrix is a vector of shape of x)

# DONE Gaussian Elimination (Ax = b)
import Matrix
if Matrix.determinant(A) != 0:
    A_inverse = Matrix.inverse(A)
    x = Matrix.vector_multiply(A, b)
else:
    raise MatrixNotInvertible

# DONE Eigenvalues (2x2 Matrix - Solving Roots of Quadratic)
import Matrix
# THEY HAD TOLD THAT THEY WONT GIVE IDENTIFIERS WHILE DEFINING CONSTANT MATRICES: Eigenvalues of 2x2 matrix [ [a, b], [c, d] ]
trace = a + d
determinant = Matrix.determinant(A)
# DONE Characteristic polynomial: λ^2 - trace * λ + determinant = 0
# DONE Solving quadratic equation using discriminant
D = trace * trace - 4 * determinant
if D >= 0:
    eigenvalue1 = (trace + sqrt(D)) / 2
    eigenvalue2 = (trace - sqrt(D)) / 2
else:
    raise ComplexEigenvaluesError


# DONE Magnitude of Matrix
import Matrix
sum_of_squares = 0
for i = 0 to A.rows-1:
    for j = 0 to A.columns-1:
        sum_of_squares := sum_of_squares + A[i, j] * A[i, j] 
magnitude_of_matrix = sqrt(sum_of_squares)


k)
# DONE Control Flow -
# DONE While Loop (magnitude function could be defined before this)
import Matrix
threshold = 1e-6
norm_diff = Matrix.magnitude(A)
while norm_diff > threshold:
    A := Matrix.multiply(A, A)
    norm_diff := Matrix.magnitude(A) - threshold

