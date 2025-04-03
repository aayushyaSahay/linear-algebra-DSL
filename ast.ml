(* 

ocamlc -c ast.ml && ocamlyacc parser.mly && ocamlc -c parser.mli parser.ml && ocamllex lexer.mll && ocamlc -c lexer.ml newmain.ml
ocamlc -o naya ./ast.cmo ./parser.cmo ./lexer.cmo ./newmain.cmo
./naya -f ./TEST1/input.txt


*)

open Printf

type types =  Bool    (* boolean *)
 | Integer
 | Float 
 | String
 | VectorInt of int
 | VectorFlo of int
 | MatrixInt of int * int
 | MatrixFlo of int * int
 | Controlok
 | ControlNone
 | IVector
 | FVector
 | IMatrix
 | FMatrix  (* current these are types without dimensions, but during type-checks, i will ensure using the AST that they get the correct types using shapes and dimensions.*)
;;

type expr =  
  | ConstB of bool (*these will not work now ->  T | F   Boolean constants *)
  | ConstI of int
  | ConstF of float    (* Float constants *)
  | ConstS of string
  | ConstIV of int * int list 
  | ConstFV of int * float list
  | ConstIM of int * int * int list list
  | ConstFM of int * int * float list list

  | ConstructIdent of string * types * expr
  | AssignIdent of string * expr
  | LookupIdent of string
  | AssignVectorAtIndex of string * expr * expr
  | AssignMatAtIndex of string * expr * expr * expr
  | LookVectAtIndex of string * expr
  | LookMatAtIndex of string * expr * expr

  | Inp of expr
  | Out of expr

  | Add of expr * expr   (* overloaded — disjunction of two booleans or sum of  two scalars or sum of two vectors/matrices of the same shape *)
  | Sub of expr * expr
  | Inv of expr     (* overloaded — negation of a boolean or additive inverse of  a scalar or additive inverse of a vector *)
  | Sqrt of expr    (* only for floats *)
  | ScalProd of expr * expr   (* overloaded — conjunction of two booleans or product of a scalar with another scalar or product of a scalar and a vector/matrix *)
  | DotProd of expr * expr  (* dot product of two vectors of the same dimension, matrix multiplication of two matrices of the same shape *)
  | Div of expr * expr
  
  | Abs of expr   (* also handles the magnitude of a vector or a determinant of a matrix *)
  
  | IsEq of expr * expr
  | IsNotEq of expr * expr
  | IsLt of expr * expr
  | IsLe of expr * expr
  | IsGt of expr * expr
  | IsGe of expr * expr
  | Modulo of expr * expr
  
  | Dim of expr
  | Angle of expr * expr  (* in radians, the angle between two vectors *)
  

  | Trans of expr
  | Det of expr
  
  | ForLoop of expr * expr * expr * expr
  | WhileLoop of expr * expr
  | IfThenElse of expr * expr * expr
  | Seq of expr * expr
  | NoOp
  | Scope of expr

  | Minor of string * expr * expr
  | Invert of expr
  | CIvect of int
  | CFvect of int
  | CImatr of int * int
  | CFmatr of int * int
  | Exit
;;


let type_to_string = function
| Bool             -> "Bool"
| Integer          -> "Integer"
| Float            -> "Float"
| String           -> "String"
| VectorInt n -> "VectorInt("^ string_of_int n ^ ")"
| VectorFlo n -> "VectorFlo("^ string_of_int n ^ ")"
| MatrixInt(n,m) -> "MatrixInt("^ string_of_int n ^ "," ^ string_of_int m ^ ")"
| MatrixFlo(n,m) -> "MatrixFlo("^ string_of_int n ^ "," ^ string_of_int m ^ ")"
| Controlok       -> "Controlok"
| ControlNone     -> "ControlNone"
| IVector         -> "IVector"
| FVector         -> "FVector"
| IMatrix         -> "IMatrix"
| FMatrix         -> "FMatrix"
;;

let rec ast_to_string = function
  | ConstB b -> sprintf "ConstB(%b)" b
  | ConstI i -> sprintf "ConstI(%d)" i
  | ConstF f -> sprintf "ConstF(%f)" f
  | ConstS s -> sprintf "ConstS(%s)" s
  | ConstIV (i, l) -> sprintf "ConstIV(%d, [%s])" i (List.fold_left (fun acc x -> acc ^ (string_of_int x) ^ "; ") "" l)
  | ConstFV (i, l) -> sprintf "ConstFV(%d, [%s])" i (List.fold_left (fun acc x -> acc ^ (string_of_float x) ^ "; ") "" l)
  | ConstIM (i, j, l) -> sprintf "ConstIM(%d, %d, [%s])" i j (List.fold_left (fun acc x -> acc  ^ (List.fold_left (fun acc y -> acc ^ (string_of_int y) ^ "; " ) "[ " x) ^ "]; ") "" l)
  | ConstFM (i, j, l) -> sprintf "ConstFM(%d, %d, [%s])" i j (List.fold_left (fun acc x -> acc  ^ (List.fold_left (fun acc y -> acc ^ (string_of_float y) ^ "; " ) "[ " x) ^ "]; ") "" l)
  | ConstructIdent (s, t, e) -> sprintf "ConstructIdent(\"%s\", %s, %s)" s (type_to_string t) (ast_to_string e)
  | AssignIdent (s, e) -> sprintf "AssignIdent(\"%s\", %s)" s (ast_to_string e)
  | LookupIdent s -> sprintf "LookupIdent(\"%s\")" s
  | Inp s -> sprintf "Inp(%s)" (ast_to_string s)
  | Out s -> sprintf "Out(%s)" (ast_to_string s)

  | Add (e1, e2) -> sprintf "Add(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | Sub (e1, e2) -> sprintf "Sub(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | Inv e -> sprintf "Inv(%s)" (ast_to_string e)
  | ScalProd (e1, e2) -> sprintf "ScalProd(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | DotProd (e1, e2) -> sprintf "DotProd(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | Div (e1, e2) -> sprintf "Div(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | Modulo (e1, e2) -> sprintf "Modulo(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | Abs e -> sprintf "Abs(%s)" (ast_to_string e)
  | IsEq (e1, e2) -> sprintf "IsEq(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | IsNotEq (e1, e2) -> sprintf "IsNotEq(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | IsLt (e1, e2) -> sprintf "IsLt(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | IsLe (e1, e2) -> sprintf "IsLe(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | IsGt (e1, e2) -> sprintf "IsGt(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | IsGe (e1, e2) -> sprintf "IsGe(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | Dim e -> sprintf "Dim(%s)" (ast_to_string e)
  | Angle (e1, e2) -> sprintf "Angle(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | Trans e -> sprintf "Trans(%s)" (ast_to_string e)
  | Det e -> sprintf "Det(%s)" (ast_to_string e)
  | ForLoop (e1, e2, e3, e4) -> sprintf "ForLoop(%s, %s, %s, %s)" (ast_to_string e1) (ast_to_string e2) (ast_to_string e3) (ast_to_string e4)
  | WhileLoop (e1, e2) -> sprintf "WhileLoop(%s, %s)" (ast_to_string e1) (ast_to_string e2)
  | IfThenElse (e1, e2, e3) -> sprintf "IfThenElse(%s, %s, %s)" (ast_to_string e1) (ast_to_string e2) (ast_to_string e3)
  | Seq (e1, e2) -> sprintf "Seq(%s, \n%s)" (ast_to_string e1) (ast_to_string e2)
  | NoOp -> "NoOp"
  | Scope(e) -> sprintf "Scope(%s)" (ast_to_string e);

  | AssignVectorAtIndex(str, e1, e2) -> sprintf "AssignVectorAtIndex(\"%s\",%s, %s)" str (ast_to_string e1) (ast_to_string e2)
  | AssignMatAtIndex(str, e1, e2, e3) -> sprintf "AssignMatAtIndex(\"%s\",%s,%s, %s)" str (ast_to_string e1) (ast_to_string e2) (ast_to_string e3)
  | LookVectAtIndex(str, e) -> sprintf "LookVectAtIndex(\"%s\",%s)" str (ast_to_string e)
  | LookMatAtIndex(str, e1, e2) -> sprintf "LookMatAtIndex(\"%s\",%s,%s)" str (ast_to_string e1) (ast_to_string e2)
  | Sqrt(e) -> sprintf "Sqrt(%s)" (ast_to_string e);
  | Minor(str,e2,e3) -> sprintf "Minor(%s,%s,%s)" str (ast_to_string e2) (ast_to_string e3)
  | Invert(e) -> sprintf "Invert(%s)" (ast_to_string e)
  | CIvect(e1) -> sprintf "CIvect(%s)" (string_of_int e1)
  | CFvect(e1) -> sprintf "CFvect(%s)" (string_of_int e1)
  | CImatr(e1,e2) -> sprintf "CImatr(%s,%s)" (string_of_int e1) (string_of_int e2)
  | CFmatr(e1,e2) -> sprintf "CFmatr(%s,%s)" (string_of_int e1) (string_of_int e2)
  | Exit -> sprintf "Exit"
;;

(* stack of scopes, where each scope is a hashtable of identifiers to types *)
type symtable = (string, types) Hashtbl.t list

(* global scope *)
let global_scope : symtable = [Hashtbl.create 16]

let enter_scope (table: symtable) : symtable = (Hashtbl.create 16) :: table

let exit_scope (table: symtable) : symtable =
  match table with
  | [] -> failwith "No scope to exit"
  | _ :: rest -> rest

let rec lookupid (table: symtable) (name: string) : types =
  match table with
  | [] -> failwith ("Did not find identifier \" " ^ name ^" \"")          
  | scope :: rest ->
      (match Hashtbl.find_opt scope name with
      | Some typ -> typ
      | None -> lookupid rest name)


let addid (table: symtable) (name: string) (typ: types) : types =
  match table with
  | [] -> failwith "No active scope"
  | current_scope :: _ ->
      if Hashtbl.mem current_scope name then
        failwith ("Identifier \" " ^ name ^ " \" already declared in this scope")
      else
        let _ = Hashtbl.add current_scope name typ
        in (Controlok)

      

let mutateid (table: symtable) (name: string) (new_value_type: types) : types =
  let existing_type =
  try 
    lookupid table name
  with 
    | _ -> failwith ("Identifier \" " ^ name ^ " \" not declared")
  in(
    if existing_type <> new_value_type then
      failwith ("Type mismatch: expected " ^ type_to_string existing_type ^ " but got " ^ type_to_string new_value_type)
    else
      Controlok
  )

(* Utility function to print current symbol table (for debugging) *)
let print_table (table: symtable) : unit =
  let print_scope scope =
    Hashtbl.iter (fun k v -> print_endline (k ^ " : " ^ type_to_string v)) scope
  in
  List.iteri (fun i scope ->
    print_endline ("Scope " ^ string_of_int i ^ ":");
    print_scope scope;
    print_endline "---"
  ) table



let rec type_of tableofIDs tree =  match tree with
| ConstB b -> Bool
| ConstI i -> Integer
| ConstF f -> Float
| ConstS s -> String
| LookupIdent s -> lookupid tableofIDs s
| ConstIV (i, l) -> let len = List.length l in (if len == i then VectorInt(i) else failwith (sprintf "Vector has dimension %d but %d elements are provided." i len))
| ConstFV (i, l) -> let len = List.length l in (if len == i then VectorFlo(i) else failwith (sprintf "Vector has dimension %d but %d elements are provided." i len))
| ConstIM (i, j, l) -> let rows = List.length l and cols = List.length (List.hd l) in 
                       if (rows == i && cols == j) then MatrixInt(i, j)
                       else failwith (sprintf "Matrix has shape(%d,%d) but got (%d,%d)." i j rows cols)
| ConstFM (i, j, l) -> let rows = List.length l and cols = List.length (List.hd l) in 
                       if (rows == i && cols == j) then MatrixFlo(i, j)
                       else failwith (sprintf "Matrix has shape(%d,%d) but got (%d,%d)." i j rows cols)
| ConstructIdent (s, t, e) -> let te = type_of tableofIDs e
                            in ( 
                              match (te,t) with
                              | Bool,Bool -> let _ = addid tableofIDs s te in Controlok
                              | Integer, Integer -> let _ = addid tableofIDs s te in Controlok
                              | Float, Float -> let _ = addid tableofIDs s te in Controlok
                              | VectorInt n, IVector -> let _ = addid tableofIDs s te in Controlok
                              | VectorFlo n, FVector -> let _ = addid tableofIDs s te in Controlok
                              | MatrixInt(n,m), IMatrix -> let _ = addid tableofIDs s te in Controlok
                              | MatrixFlo(n,m), FMatrix -> let _ = addid tableofIDs s te in Controlok
                              | Controlok, _ -> let _ = addid tableofIDs s t in Controlok
                              | _,_ -> failwith (sprintf "Identifier %s was declared to be of type %s but is being assigned an expression of type %s." s (type_to_string t) (type_to_string te));
                              )
| AssignIdent (s, e) -> (match e with 
                          | Inp _  -> Controlok
                          | _ -> (let te = type_of tableofIDs e 
                                  in 
                                  let _ = mutateid tableofIDs s te in Controlok))
| Inp s -> type_of tableofIDs s
| Out s -> (match s with 
          | LookupIdent(x) -> let _ = type_of tableofIDs s in Controlok
          | ConstS str -> Controlok
          | _ -> failwith (sprintf "Arguments of Out should be either a string or an identifier, provided %s." (type_to_string (type_of tableofIDs s))))
| Add (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                        match (t1,t2) with
                          | Bool,Bool -> Bool
                          | Integer, Integer -> Integer
                          | Float, Float -> Float
                          | VectorInt n, VectorInt m when n == m -> VectorInt n
                          | VectorFlo n, VectorFlo m when n == m -> VectorFlo n
                          | MatrixInt(n1,m1), MatrixInt(n2,m2) when n1 == m1 && n2 == m2 -> MatrixInt(n1,m1)
                          | MatrixFlo(n1,m1), MatrixFlo(n2,m2) when n1 == m1 && n2 == m2 -> MatrixFlo(n1,m1)
                          | _,_ -> failwith (sprintf "Incompatible types of arguments of Add: %s and %s" (type_to_string t1) (type_to_string t2))
                        )
| Sub (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                        match (t1,t2) with
                          | Integer, Integer -> Integer
                          | Float, Float -> Float
                          | VectorInt n, VectorInt m when n == m -> VectorInt n
                          | VectorFlo n, VectorFlo m when n == m -> VectorFlo n
                          | MatrixInt(n1,m1), MatrixInt(n2,m2) when n1 == m1 && n2 == m2 -> MatrixInt(n1,m1)
                          | MatrixFlo(n1,m1), MatrixFlo(n2,m2) when n1 == m1 && n2 == m2 -> MatrixFlo(n1,m1)
                          | _,_ -> failwith (sprintf "Incompatible types of arguments of Sub: %s and %s" (type_to_string t1) (type_to_string t2))
                        )
| Inv e -> let t1 = (type_of tableofIDs e) in (
                    match t1 with
                    | Bool -> Bool
                    | Integer -> Integer
                    | Float -> Float
                    | VectorInt n -> VectorInt n
                    | VectorFlo n -> VectorFlo n
                    | MatrixInt(n1,m1) -> MatrixInt(n1,m1)
                    | MatrixFlo(n1,m1) -> MatrixFlo(n1,m1)
                    | _ -> failwith (sprintf "Invalid argument type of Inv: %s." (type_to_string t1))
                  )
| ScalProd (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                    match (t1,t2) with
                      | Bool,Bool -> Bool
                      | Integer, Integer -> Integer
                      | Float, Float -> Float
                      | Integer, VectorInt n -> VectorInt n
                      | VectorInt n, Integer -> VectorInt n
                      | Float, VectorFlo n -> VectorFlo n
                      | VectorFlo n, Float -> VectorFlo n
                      | Integer, MatrixInt(n1,m1) -> MatrixInt(n1,m1)
                      | MatrixInt(n1,m1), Integer -> MatrixInt(n1,m1)
                      | Float, MatrixFlo(n1,m1) -> MatrixFlo(n1,m1)
                      | MatrixFlo(n1,m1), Float -> MatrixFlo(n1,m1)
                      | _,_ -> failwith (sprintf "Incompatible types of arguments of ScalProd: %s and %s" (type_to_string t1) (type_to_string t2))
                    )
| DotProd (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                    match (t1,t2) with
                      | VectorInt n, VectorInt m when n == m -> Integer
                      | VectorFlo n, VectorFlo m when n == m -> Float
                      | MatrixInt(n1,m1), VectorInt(n) when n1 == n -> VectorInt(n) 
                      | MatrixFlo(n1,m1), VectorFlo(n) when n1 == n -> VectorFlo(n) 
                      | MatrixInt(n1,m1), MatrixInt(n2,m2) when (m1 == n2) -> MatrixInt(n1,m2)
                      | MatrixFlo(n1,m1), MatrixFlo(n2,m2) when (m1 == n2) -> MatrixFlo(n1,m2)
                      | _,_ -> failwith (sprintf "Incompatible types of arguments of DotProd: %s and %s" (type_to_string t1) (type_to_string t2))
                    )
(* Division by zero cannot be checked here because we haven't yet devised a way to calculate the value of any expression. *)
| Div (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                  match (t1,t2) with
                    | Integer, Integer -> Integer
                    | Float, Float -> Float
                    | _,_ -> failwith (sprintf "Incompatible types of arguments of Div: %s and %s" (type_to_string t1) (type_to_string t2))
                  )
| Modulo (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                    match (t1,t2) with
                      | Integer, Integer -> Integer
                      | _,_ -> failwith (sprintf "Incompatible types of arguments of Modulo: %s and %s" (type_to_string t1) (type_to_string t2))
                    )
| Abs e -> let t1 = (type_of tableofIDs e) in (
                match t1 with
                | Integer -> Integer
                | Float -> Float
                | VectorInt n -> Float
                | VectorFlo n -> Float
                | _ -> failwith (sprintf "Invalid argument type of Abs: %s." (type_to_string t1))
              )
| IsEq (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                      match (t1,t2) with
                        | Bool,Bool -> Bool
                        | Integer, Integer -> Bool
                        | Float, Float -> Bool
                        | VectorInt n, VectorInt m when n == m -> Bool
                        | VectorFlo n, VectorFlo m when n == m -> Bool
                        | MatrixInt(n1,m1), MatrixInt(n2,m2) when n1 == m1 && n2 == m2 -> Bool
                        | MatrixFlo(n1,m1), MatrixFlo(n2,m2) when n1 == m1 && n2 == m2 -> Bool
                        | _,_ -> failwith (sprintf "Incompatible types of arguments of IsEq: %s and %s" (type_to_string t1) (type_to_string t2))
                      )
| IsNotEq(e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                      match (t1,t2) with
                        | Bool,Bool -> Bool
                        | Integer, Integer -> Bool
                        | Float, Float -> Bool
                        | VectorInt n, VectorInt m when n == m -> Bool
                        | VectorFlo n, VectorFlo m when n == m -> Bool
                        | MatrixInt(n1,m1), MatrixInt(n2,m2) when n1 == m1 && n2 == m2 -> Bool
                        | MatrixFlo(n1,m1), MatrixFlo(n2,m2) when n1 == m1 && n2 == m2 -> Bool
                        | _,_ -> failwith (sprintf "Incompatible types of arguments of IsEq: %s and %s" (type_to_string t1) (type_to_string t2))
                      )
| IsLt (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                      match (t1,t2) with
                        | Integer, Integer -> Bool
                        | Float, Float -> Bool
                        | _,_ -> failwith (sprintf "Incompatible types of arguments of IsLt: %s and %s" (type_to_string t1) (type_to_string t2))
                      )
| IsLe (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                      match (t1,t2) with
                        | Integer, Integer -> Bool
                        | Float, Float -> Bool
                        | _,_ -> failwith (sprintf "Incompatible types of arguments of IsLe: %s and %s" (type_to_string t1) (type_to_string t2))
                      )
| IsGt (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                      match (t1,t2) with
                        | Integer, Integer -> Bool
                        | Float, Float -> Bool
                        | _,_ -> failwith (sprintf "Incompatible types of arguments of IsGt: %s and %s" (type_to_string t1) (type_to_string t2))
                      )
| IsGe (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                      match (t1,t2) with
                        | Integer, Integer -> Bool
                        | Float, Float -> Bool
                        | _,_ -> failwith (sprintf "Incompatible types of arguments of IsGe: %s and %s" (type_to_string t1) (type_to_string t2))
                      )
| Dim e -> let t1 = (type_of tableofIDs e) in (
            match t1 with
            | VectorInt n -> Integer
            | VectorFlo n -> Integer
            | _ -> failwith (sprintf "Invalid argument type of Dim: %s." (type_to_string t1))
          )
| Angle (e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (
                      match (t1,t2) with
                        | VectorInt n, VectorInt m when n == m -> Float
                        | VectorFlo n, VectorFlo m when n == m -> Float
                        | _,_ -> failwith (sprintf "Incompatible types of arguments of Angle: %s and %s" (type_to_string t1) (type_to_string t2))
                      )
| Trans e -> let t1 = (type_of tableofIDs e) in (
                  match t1 with
                  | MatrixInt(n1,m1) -> MatrixInt(m1,n1)
                  | MatrixFlo(n1,m1) -> MatrixFlo(m1,n1)
                  | _ -> failwith (sprintf "Invalid argument type of Inv: %s." (type_to_string t1))
                )
| Det e -> let t1 = (type_of tableofIDs e) in (
                  match t1 with
                  | MatrixInt(n1,m1) when n1 == m1 -> Integer
                  | MatrixFlo(n1,m1) when n1 == m1 -> Float
                  | _ -> failwith (sprintf "Invalid argument type of Det: %s." (type_to_string t1))
                )
| ForLoop (e1, e2, e3, e4) -> let _ = type_of tableofIDs e1 and t1 = type_of tableofIDs e2 and __ = type_of tableofIDs e3 and _ = type_of tableofIDs e4 in (
                    match t1 with 
                    | Bool -> Controlok
                    | _-> failwith (sprintf "There should be a Bool in the second block, got %s (ForLoop)" (type_to_string t1))
                )
| WhileLoop (e1, e2) -> let t1 = type_of tableofIDs e1 and _ = type_of tableofIDs e2 in (
                    match t1 with 
                    | Bool -> Controlok
                    | _ -> failwith (sprintf "There should be a Bool in the first block, got %s (WhileLoop)" (type_to_string t1))
                  )
| IfThenElse (e1, e2, e3) -> let t1 = (type_of tableofIDs e1) in (match t1 with
                    | Bool -> (let t2 = (type_of tableofIDs e2) and t3 = (type_of tableofIDs e3) in (
                                  if(t2 == t3) then Controlok else failwith (sprintf "Arguments are not of the same type, got %s %s" (type_to_string t2) (type_to_string t3))))
                    | _ -> failwith (sprintf "First block should be of type Bool, got %s" (type_to_string t1))
                    )
| Seq (e1, e2) -> let _ = (type_of tableofIDs e1) and _ = (type_of tableofIDs e2) in Controlok
| NoOp -> Controlok
| Scope(e) -> let _ = type_of (enter_scope tableofIDs) e in (Controlok)


| AssignVectorAtIndex(str, e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (match t1 with 
                                      | Integer -> (
                                        let existing_type =
                                          try 
                                            lookupid tableofIDs str
                                          with 
                                            | _ -> failwith ("Identifier \" " ^ str ^ " \" not declared")
                                          in(
                                            match (existing_type,t2) with 
                                            | VectorFlo(_),Float -> Controlok 
                                            | VectorInt(_),Integer -> Controlok
                                            | _ -> failwith ("Type mismatch: expected " ^ type_to_string existing_type ^ " but got " ^ type_to_string t2)
                                            )
                                        )
                                        | _ -> failwith (sprintf "Index should be of integer type, is of %s" (type_to_string t1)))
| AssignMatAtIndex(str, e1, e2, e3) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) and t3 = (type_of tableofIDs e3) in (match (t1,t2) with 
                                      | Integer,Integer -> (
                                        let existing_type =
                                          try 
                                            lookupid tableofIDs str
                                          with 
                                            | _ -> failwith ("Identifier \" " ^ str ^ " \" not declared")
                                          in(
                                            (match (existing_type,t3) with 
                                            | MatrixFlo(_,_),Float -> Controlok 
                                            | MatrixInt(_,_),Integer -> Controlok
                                            | _ -> failwith ("Type mismatch: expected " ^ type_to_string existing_type ^ " but got " ^ type_to_string t3)
                                            )
                                          )
                                      )
                                      | _ -> failwith (sprintf "Index should be of (integer,integer) type, is of (%s,%s)" (type_to_string t1)(type_to_string t2)))
| LookVectAtIndex(str, e) -> let t1 = (type_of tableofIDs e) in (match t1 with 
                                      | Integer -> let tv = lookupid tableofIDs str in (match tv with 
                                                                                        | VectorInt(_) -> Integer
                                                                                        | VectorFlo(_) -> Float
                                                                                        | _ -> failwith "Invalid Indexing"
                                                                                      )
                                      | _ -> failwith (sprintf "Index should be of integer type, is of %s" (type_to_string t1)))
| LookMatAtIndex(str, e1, e2) -> let t1 = (type_of tableofIDs e1) and t2 = (type_of tableofIDs e2) in (match (t1,t2) with 
                                      | Integer,Integer -> let tm = lookupid tableofIDs str in (match tm with 
                                                                                                | MatrixInt(_,_) -> Integer
                                                                                                | MatrixFlo(_,_) -> Float
                                                                                                | _ -> failwith "Invalid Indexing")
                                      | _ -> failwith (sprintf "Index should be of (integer,integer) type, is of (%s,%s)" (type_to_string t1)(type_to_string t2)))
| Sqrt(e) -> let t = type_of tableofIDs e in (match t with Float -> Float | _ -> failwith (sprintf "Argument of sqrt shoudl be a Float, but was provided %s" (type_to_string t)));
| Minor(str,e2,e3) -> let t1 = lookupid tableofIDs str and t2 = type_of tableofIDs e2 and t3 = type_of tableofIDs e3 in (match (t1,t2,t3) with 
                          | MatrixInt(n1,n2),Integer,Integer -> MatrixInt(n1-1,n2-1)
                          | MatrixFlo(n1,n2),Integer,Integer -> MatrixFlo(n1-1,n2-1)
                          | _ -> failwith (sprintf "Arguments of Minor should be MatrixInt()/MatrixFlo(), Integer, Integer; but were provided: (%s,%s,%s)" (type_to_string t1) (type_to_string t2) (type_to_string t3))
                          )
| Invert(e) -> let t = type_of tableofIDs e in (match t with 
                    | MatrixInt(n1,n2) -> MatrixInt(n1,n2) 
                    | MatrixFlo(n1,n2) -> MatrixFlo(n1,n2) 
                    | _ -> failwith (sprintf "Argument of Invert shoudl be a MatrixInt()/MatrixFlo(), but was provided %s" (type_to_string t)));
| CIvect(e) -> VectorInt(e)
| CFvect(e) -> VectorFlo(e)
| CImatr(e1,e2) -> MatrixInt(e1,e2)
| CFmatr(e1,e2) -> MatrixFlo(e1,e2)
| Exit -> Controlok

;;





let e2 = Seq(Scope(Seq(ConstructIdent("a", Integer, ConstI(5)), 
Seq(Scope(Seq(ConstructIdent("a", Integer, ConstB(false)), 
Seq(Out(LookupIdent("a")), 
NoOp))), 
Seq(Out(LookupIdent("a")), 
NoOp)))), 
NoOp)
;;

let e3 = Seq(Scope(Seq(ConstructIdent("a", Integer, ConstI(5)), 
Seq(Scope(Seq(ConstructIdent("a", Integer, ConstI(6)), 
Seq(Out(LookupIdent("b")), 
NoOp))), 
Seq(Out(LookupIdent("a")), 
NoOp)))), 
NoOp)
;;

let e4 = Seq(Scope(Seq(ConstructIdent("a", Integer, ConstI(5)), 
Seq(Scope(Seq(ConstructIdent("a", Bool, ConstB(false)), 
Seq(Out(LookupIdent("a")), 
NoOp))), 
Seq(Out(LookupIdent("a")), 
NoOp)))), 
NoOp)
;;
let e5 = ConstructIdent("firstint", Integer, Inp(NoOp));;
(* type_of global_scope e5;; *)