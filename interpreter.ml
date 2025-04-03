open Printf
open Ast
open Parser
open Lexer

type values = 
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | VectI of int * int list 
  | VectF of int * float list
  | MatrI of int * int * int list list
  | MatrF of int * int * float list list
  | Unit
;;


(* stack of scopes, where each scope is a hashtable of identifiers to types *)
type symtable = (string, values) Hashtbl.t list
;;


let int_enter_scope (table: symtable) : symtable = (Hashtbl.create 16) :: table
;;

let int_exit_scope (table: symtable) : symtable =
  match table with
  | [] -> failwith "No scope to exit"
  | _ :: rest -> rest
;;

let rec getvaluID (table: symtable) (name: string) : values =
  match table with
  | [] -> failwith ("Did not find identifier \" " ^ name ^" \"")          
  | scope :: rest ->
    (match Hashtbl.find_opt scope name with
    | Some valu -> valu
    | None -> getvaluID rest name)
  ;;
  
  let addvaluID (table: symtable) (name: string) (valu: values) : values =
    match table with
    | [] -> failwith "No active scope"
    | current_scope :: _ ->
      if Hashtbl.mem current_scope name then
        failwith ("Identifier \" " ^ name ^ " \" already declared in this scope")
      else
        let _ = Hashtbl.add current_scope name valu
      in (Unit)
    ;;
    
    let mutvaluID (table: symtable) (name: string) (new_value: values) : values =
      let rec helper_mutate table =
        match table with
        | [] -> failwith ("Identifier \" " ^ name ^ " \" not declared")
    | scope :: rest ->
      if Hashtbl.mem scope name then
        let _ = Hashtbl.replace scope name new_value in
        Unit
      else
        helper_mutate rest
      in
      helper_mutate table
    ;;


(* global scope *)
let global_scope : symtable = [Hashtbl.create 16]
;;
    
(* Utility function to print current symbol table (for debugging) *)
(* let print_table (table: symtable) : unit =
  let print_scope scope =
    Hashtbl.iter (fun k v -> print_endline (k ^ " : " ^ type_to_string v)) scope
  in
  List.iteri (fun i scope ->
    print_endline ("Scope " ^ string_of_int i ^ ":");
    print_scope scope;
    print_endline "---"
    ) table *)
    
    exception Wrong of string;;
    let epsilon = 1e-6;;
let absol x = if x < 0.0 then -.x else x;;
let compwithEPS x = if (absol x) <= epsilon then true else false;;
let my_reverse s = List.fold_left (fun curr_val x -> x :: curr_val) [] s;;
let createint : int -> int -> int list = fun n x ->
  let rec helper_create count vect_so_far =
    if count = 0 then vect_so_far
    else helper_create (count-1) (x :: vect_so_far)
  in
  if n < 1 then raise (Wrong("createint"))
  else helper_create n [];;
let createfloat : int -> float -> float list = fun n x ->
  let rec helper_create count vect_so_far =
    if count = 0 then vect_so_far
    else helper_create (count-1) (x :: vect_so_far)
  in
  if n < 1 then raise (Wrong("createfloat"))
  else helper_create n [];;
let dim = fun l ->
  (if (l=[]) then raise (Wrong("dim"))
  else Int(List.fold_left (fun curr_val _ -> curr_val + 1) 0 l));;
let is_zero_int= fun v ->
  let rec helper_is_zero v = match v with
    | [] -> false
    | x :: xs -> if(x <> 0) then helper_is_zero xs else false
  in
  if (v=[]) then raise (Wrong("is_zero_int"))
  else helper_is_zero v;;
let is_zero_float= fun v ->
  let rec helper_is_zero v = match v with
    | [] -> false
    | x :: xs -> if(absol x < epsilon) then helper_is_zero xs else false
  in
  if (v=[]) then raise (Wrong("is_zero_float"))
  else helper_is_zero v;;
let unit_int = fun n j ->
  let rec helper_unit n j list_so_far =
    match n with
    | 0 -> list_so_far
    | y -> if y = j then helper_unit (n - 1) j (1 :: list_so_far) else helper_unit (n - 1) j (0 :: list_so_far)
  in
  if (j < 1 || n < 1 || j > n) then 
    raise (Wrong("unit_int"))
  else helper_unit n j [];;
let unit_float = fun n j ->
  let rec helper_unit n j list_so_far =
    match n with
    | 0 -> list_so_far
    | y -> if y = j then helper_unit (n - 1) j (1.0 :: list_so_far) else helper_unit (n - 1) j (0.0 :: list_so_far)
  in
  if (j < 1 || n < 1 || j > n) then 
    raise (Wrong("unit_float"))
  else helper_unit n j [];;
let scale_int = fun c v ->
  if v=[] then raise (Wrong("sacle_int"))
  else List.map (fun x -> c * x) v;;
let scale_float = fun c v ->
  if v=[] then raise (Wrong("scale_float"))
  else List.map (fun x -> c *. x) v;;
let addv_int = fun v1 v2 ->
  let rec helper_addv v1 v2 new_vector =
    match (v1,v2) with
    | ([],[]) -> new_vector
    | (x :: xs,y::ys) -> helper_addv xs ys ((x+y)::new_vector)
    | _,_ -> [0]
  in
  if((dim v1) <> (dim v2)) then raise (Wrong("addv_int"))
  else my_reverse (helper_addv v1 v2 []);;
let addv_float = fun v1 v2 ->
  let rec helper_addv v1 v2 new_vector =
    match (v1,v2) with
    | ([],[]) -> new_vector
    | (x :: xs,y::ys) -> helper_addv xs ys ((x+.y)::new_vector)
    | _,_ -> [0.0]
  in
  if((dim v1) <> (dim v2)) then raise (Wrong("addv_float"))
  else my_reverse (helper_addv v1 v2 []);;
let dot_prod_int = fun v1 v2 ->
  let rec helper_dot_prod v1 v2 prod_so_far =
    match (v1,v2) with
    | ([],[]) -> prod_so_far
    | (x :: xs, y :: ys) -> helper_dot_prod xs ys ((x * y) + prod_so_far)
    | _ ,_ -> 0
  in
  if((dim v1) <> (dim v2)) then raise (Wrong("dot_prod_int"))
  else helper_dot_prod v1 v2 0;;
let dot_prod_float = fun v1 v2 ->
  let rec helper_dot_prod v1 v2 prod_so_far =
    match (v1,v2) with
    | ([],[]) -> prod_so_far
    | (x :: xs, y :: ys) -> helper_dot_prod xs ys ((x *. y) +. prod_so_far)
    | _ ,_ -> 0.0
  in
  if((dim v1) <> (dim v2)) then raise (Wrong("dot_prod_float"))
  else helper_dot_prod v1 v2 0.0;;
let inv_int = fun v -> scale_int (-1) v;;
let inv_float = fun v -> scale_float (-1.0) v;;
let length_int= fun v ->
  if(v=[]) then raise (Wrong("length_int"))
  else Float.sqrt((float)(List.fold_left (fun curr_val x -> curr_val + x*x) 0 v));;
let length_float = fun v ->
  if(v=[]) then raise (Wrong("length_float"))
  else Float.sqrt(List.fold_left (fun curr_val x -> curr_val +. x*.x) 0.0 v);;
let angle_int = fun v1 v2 ->
  let rem_innacuracy x =
    if x < -1.0 then -1.0
    else if x > 1.0 then 1.0
    else x
  in
  if((dim v1) <> (dim v2)) then raise (Wrong("angle_int dim"))
  else if (is_zero_int v1 || is_zero_int v2) then raise (Wrong("angle_int zero"))
  else Float.acos(rem_innacuracy ((float)(dot_prod_int v1 v2)/.(length_int v1 *. length_int v2)));;
;;
let angle_float = fun v1 v2 ->
  let rem_innacuracy x =
    if x < -1.0 then -1.0
    else if x > 1.0 then 1.0
    else x
  in
  if((dim v1) <> (dim v2)) then raise (Wrong("angle_float"))
  else if (is_zero_float v1 || is_zero_float v2) then raise (Wrong("angle_float"))
  else Float.acos(rem_innacuracy ((dot_prod_float v1 v2)/.(length_float v1 *. length_float v2)));;
;;


let rec eval tableofIDs tree =  match tree with
| ConstB b -> Bool b
| ConstI i -> Int i
| ConstF f -> Float f
| ConstS s -> String (String.sub s 1 (String.length s - 2))
| LookupIdent s -> getvaluID tableofIDs s
| ConstIV (i, l) -> let len = List.length l in (if len == i then VectI(i,l) else failwith (sprintf "Vector has dimension %d but %d elements are provided." i len))
| ConstFV (i, l) -> let len = List.length l in (if len == i then VectF(i,l) else failwith (sprintf "Vector has dimension %d but %d elements are provided." i len))
| ConstIM (i, j, l) -> let rows = List.length l and cols = List.length (List.hd l) in 
                       if (rows == i && cols == j) then MatrI(i,j,l)
                       else failwith (sprintf "Matrix has shape(%d,%d) but got (%d,%d)." i j rows cols)
| ConstFM (i, j, l) -> let rows = List.length l and cols = List.length (List.hd l) in 
                       if (rows == i && cols == j) then MatrF(i,j,l)
                       else failwith (sprintf "Matrix has shape(%d,%d) but got (%d,%d)." i j rows cols)
| ConstructIdent (s, t, e) -> let te = eval tableofIDs e in ( 
                              match te with
                              | Bool _ -> let _ = addvaluID tableofIDs s te in Unit
                              | Int _ -> let _ = addvaluID tableofIDs s te in Unit
                              | Float _ -> let _ = addvaluID tableofIDs s te in Unit
                              | VectI(_) -> let _ = addvaluID tableofIDs s te in Unit
                              | VectF(_) -> let _ = addvaluID tableofIDs s te in Unit
                              | MatrI(_) -> let _ = addvaluID tableofIDs s te in Unit
                              | MatrF(_) -> let _ = addvaluID tableofIDs s te in Unit
                              | _ -> failwith (sprintf "How did it come through the type-checker to the ConstructIdent function?")
                              )
| AssignIdent (s, e) -> let te = (eval tableofIDs e) in (
                                let _ = mutvaluID tableofIDs s te in Unit)
| Inp s -> ( (* currently stdin supported, file support not there, might need to add a file handle in order to recognise the difference and read from a file instead *)
            match s with 
            | ConstS str -> (
                    let stripped_str = String.sub str 1 (String.length str - 2) in
                    let input_channel = open_in stripped_str in
                    let parsed_expr = Parser.exp Lexer.segmentar (Lexing.from_channel input_channel) in
                    close_in input_channel;
                    let _ = Ast.type_of [Hashtbl.create 16] parsed_expr in
                    eval tableofIDs parsed_expr
                )
            | NoOp -> (
                    let input = read_line () in
                    let parsed_expr = Parser.exp Lexer.segmentar (Lexing.from_string input) in
                    let _ = Ast.type_of [Hashtbl.create 16] parsed_expr in
                    eval tableofIDs parsed_expr
                )
            | _ -> failwith (sprintf "How did it come through the type-checker to the Inp function?")
           )
| Out s -> 
  let value = eval tableofIDs s in 
  ((match value with
  | String str -> (print_string str)
  | Bool b -> (print_endline (string_of_bool b))
  | Int i -> (print_endline (string_of_int i))
  | Float f -> (print_endline (string_of_float f))
  | VectI(_, l) -> (print_endline ("[" ^ (String.concat ", " (List.map string_of_int l)) ^ "]"))
  | VectF(_, l) -> (print_endline ("[" ^ (String.concat ", " (List.map string_of_float l)) ^ "]"))
  | MatrI(_, _, l) -> 
    (let rows = List.map (fun row -> "\t[" ^ (String.concat ", " (List.map string_of_int row)) ^ "]") l in
    print_endline ("[\n" ^ (String.concat "\n" rows) ^ "\n]"))
  | MatrF(_, _, l) -> 
    (let rows = List.map (fun row -> "\t[" ^ (String.concat ", " (List.map string_of_float row)) ^ "]") l in
    print_endline ("[\n" ^ (String.concat "\n" rows) ^ "\n]"))
  | Unit -> ());
  flush stdout;
  Unit
  )
| Add (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                        match (t1,t2) with
                          | Bool b1 ,Bool b2-> Bool (b1 || b2)
                          | Int n1, Int n2 -> Int (n1+n2)
                          | Float f1, Float f2 -> Float (f1 +. f2)
                          | VectI(n1,l1), VectI(n2,l2) when n1 == n2 -> VectI(n1, addv_int l1 l2)
                          | VectF(n1,l1), VectF(n2,l2) when n1 == n2 -> VectF(n1, addv_float l1 l2)
                          | MatrI(n1, m1, l1), MatrI(n2, m2, l2) when n1 == n2 && m1 == m2 -> (
                                                                          let rec add_rows rows1 rows2 =
                                                                          match (rows1, rows2) with
                                                                          | ([], []) -> []
                                                                          | (r1 :: rs1, r2 :: rs2) -> (addv_int r1 r2) :: (add_rows rs1 rs2)
                                                                          | _ -> failwith "Matrices are of unequal dimensions"
                                                                          in
                                                                          MatrI(n1, m1, add_rows l1 l2)
                                                                        )
                          | MatrF(n1, m1, l1), MatrF(n2, m2, l2) when n1 == n2 && m1 == m2 -> (
                                                                        let rec add_rows rows1 rows2 =
                                                                        match (rows1, rows2) with
                                                                        | ([], []) -> []
                                                                        | (r1 :: rs1, r2 :: rs2) -> (addv_float r1 r2) :: (add_rows rs1 rs2)
                                                                        | _ -> failwith "Matrices are of unequal dimensions"
                                                                        in
                                                                        MatrF(n1, m1, add_rows l1 l2)
                                                                      )
                          | _,_ -> failwith (sprintf "How did it come through the type-checker to the Add function?")
                        )
| Sub (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                        match (t1,t2) with
                        | Int n1, Int n2 -> Int (n1-n2)
                        | Float f1, Float f2 -> Float (f1 -. f2)
                        | VectI(n1,l1), VectI(n2,l2) when n1 == n2 -> (
                                                                        let rec add_lists l1 l2 =
                                                                          match (l1, l2) with
                                                                          | ([], []) -> []
                                                                          | (x1 :: xs1, x2 :: xs2) -> (x1 - x2) :: (add_lists xs1 xs2)
                                                                          | _ -> failwith "Lists are of unequal length"
                                                                        in
                                                                        VectI(n1, add_lists l1 l2)
                                                                      )
                        | VectF(n1,l1), VectF(n2,l2) when n1 == n2 -> (
                                                                    let rec add_lists l1 l2 =
                                                                      match (l1, l2) with
                                                                      | ([], []) -> []
                                                                      | (x1 :: xs1, x2 :: xs2) -> (x1 -. x2) :: (add_lists xs1 xs2)
                                                                      | _ -> failwith "Lists are of unequal length"
                                                                    in
                                                                    VectF(n1, add_lists l1 l2)
                                                                  )
                        | MatrI(n1, m1, l1), MatrI(n2, m2, l2) when n1 == n2 && m1 == m2 -> (
                                                                        let rec add_rows rows1 rows2 =
                                                                        match (rows1, rows2) with
                                                                        | ([], []) -> []
                                                                        | (r1 :: rs1, r2 :: rs2) ->
                                                                          let rec add_cells c1 c2 =
                                                                            match (c1, c2) with
                                                                            | ([], []) -> []
                                                                            | (x1 :: xs1, x2 :: xs2) -> (x1 - x2) :: (add_cells xs1 xs2)
                                                                            | _ -> failwith "Rows are of unequal length"
                                                                          in
                                                                          (add_cells r1 r2) :: (add_rows rs1 rs2)
                                                                        | _ -> failwith "Matrices are of unequal dimensions"
                                                                        in
                                                                        MatrI(n1, m1, add_rows l1 l2)
                                                                      )
                        | MatrF(n1, m1, l1), MatrF(n2, m2, l2) when n1 == n2 && m1 == m2 -> (
                                                                      let rec add_rows rows1 rows2 =
                                                                      match (rows1, rows2) with
                                                                      | ([], []) -> []
                                                                      | (r1 :: rs1, r2 :: rs2) ->
                                                                        let rec add_cells c1 c2 =
                                                                          match (c1, c2) with
                                                                          | ([], []) -> []
                                                                          | (x1 :: xs1, x2 :: xs2) -> (x1 -. x2) :: (add_cells xs1 xs2)
                                                                          | _ -> failwith "Rows are of unequal length"
                                                                        in
                                                                        (add_cells r1 r2) :: (add_rows rs1 rs2)
                                                                      | _ -> failwith "Matrices are of unequal dimensions"
                                                                      in
                                                                      MatrF(n1, m1, add_rows l1 l2)
                                                                    )
                        | _,_ -> failwith (sprintf "How did it come through the type-checker to the Sub function?")
                      )
| Inv e -> let t1 = (eval tableofIDs e) in (
                    match t1 with
                    | Bool b -> Bool (not b)
                    | Int n1 -> Int (-1*n1)
                    | Float f -> Float (-1.0 *. f)
                    | VectI(n, l) -> VectI(n, inv_int l)
                    | VectF(n, l) -> VectF(n, inv_float l)
                    | MatrI(n, m, l) -> MatrI(n, m, List.map (fun row -> inv_int row) l)
                    | MatrF(n, m, l) -> MatrF(n, m, List.map (fun row -> inv_float row) l)
                    | _ -> failwith (sprintf "How did it come through the type-checker to the Inv function!?")
                  )
| ScalProd (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                    match (t1,t2) with
                      | Bool b1,Bool b2 -> Bool (b1 && b2)
                      | Int n1, Int n2 -> Int (n1*n2)
                      | Float f1, Float f2 -> Float (f1 *. f2)
                      | Int n1, VectI(n,l) -> VectI(n, List.map (fun x -> n1 * x) l)
                      | VectI(n,l), Int n1 -> VectI(n, List.map (fun x -> n1 * x) l)
                      | Float f, VectF(n,l) -> VectF(n, List.map (fun x -> f *. x) l)
                      | VectF(n,l), Float f -> VectF(n, List.map (fun x -> f *. x) l)

                      | Int n1, MatrI(n,m,l) -> MatrI(n, m, List.map (fun row -> List.map (fun x -> n1 * x) row) l)
                      | MatrI(n,m,l), Int n1 -> MatrI(n, m, List.map (fun row -> List.map (fun x -> n1 * x) row) l)
                      | Float f, MatrF(n,m,l) -> MatrF(n, m, List.map (fun row -> List.map (fun x -> f *. x) row) l)
                      | MatrF(n,m,l), Float f -> MatrF(n, m, List.map (fun row -> List.map (fun x -> f *. x) row) l)
                      | _,_ -> failwith (sprintf "How did it come through the type-checker to the ScalProd function!?" )
                    )
| DotProd (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                      match (t1, t2) with
                      | VectI(n, l1), VectI(m, l2) when n = m -> 
                        let result = List.fold_left2 (fun acc x y -> acc + (x * y)) 0 l1 l2 in
                        Int result
                      | VectF(n, l1), VectF(m, l2) when n = m -> 
                        let result = List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 l1 l2 in
                        Float result
                      | MatrI(n1, m1, l1), VectI(n, l2) when m1 = n -> 
                        let result = List.map (fun row -> List.fold_left2 (fun acc x y -> acc + (x * y)) 0 row l2) l1 in
                        VectI(n1, result)
                      | MatrF(n1, m1, l1), VectF(n2, l2) when m1 = n2 -> 
                        let result = List.map (fun row -> List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 row l2) l1 in
                        VectF(n1, result)
                      | MatrI(n1, m1, l1), MatrI(n2, m2, l2) when m1 = n2 -> 
                        let l2_transposed = List.map (fun i -> List.map (fun row -> List.nth row i) l2) (List.init m2 (fun i -> i)) in
                        let result = List.map (fun row1 -> 
                          List.map (fun col2 -> 
                            List.fold_left2 (fun acc x y -> acc + (x * y)) 0 row1 col2
                          ) l2_transposed
                        ) l1 in
                        MatrI(n1, m2, result)
                      | MatrF(n1, m1, l1), MatrF(n2, m2, l2) when m1 = n2 -> 
                        let l2_transposed = List.map (fun i -> List.map (fun row -> List.nth row i) l2) (List.init m2 (fun i -> i)) in
                        let result = List.map (fun row1 -> 
                          List.map (fun col2 -> 
                            List.fold_left2 (fun acc x y -> acc +. (x *. y)) 0.0 row1 col2
                          ) l2_transposed
                        ) l1 in
                        MatrF(n1, m2, result)
                      | _, _ -> failwith (sprintf "How did it come through the type-checker to the DotProd function!?" )
                      )
(* Division by zero cannot be checked here because we haven't yet devised a way to calculate the value of any expression. *)
| Div (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                      match (t1,t2) with
                        | Int n1, Int n2 when (n2<>0) -> Int (n1/n2)
                        | Float f1, Float f2 when (abs_float(f2) > 0.000001)  -> Float (f1/.f2)
                        | _,_ -> failwith (sprintf "How did it come through the type-checker to the Div function!?" )
                      )
| Modulo (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                      match (t1,t2) with
                        | Int n1, Int n2 when (n2 <> 0) -> Int (n1 mod n2)
                        | _,_ -> failwith (sprintf "How did it come through the type-checker to the Modulo function!?" )
                      )
| Abs e -> let t1 = (eval tableofIDs e) in (
                      match t1 with
                      | Int n1 -> Int(abs n1)
                      | Float f -> Float(abs_float f)
                      | VectI(n, l) -> 
                          let sum_of_squares = List.fold_left (fun acc x -> acc + (x * x)) 0 l in
                          Float (sqrt (float_of_int sum_of_squares))
                      | VectF(n, l) -> 
                          let sum_of_squares = List.fold_left (fun acc x -> acc +. (x *. x)) 0.0 l in
                          Float (sqrt sum_of_squares)
                      | _ -> failwith (sprintf "How did it come through the type-checker to the Abs function!?" )
                    )
| IsEq (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                      match (t1, t2) with
                      | Bool b1, Bool b2 -> Bool (b1 = b2)
                      | Int n1, Int n2 -> Bool (n1 = n2)
                      | Float f1, Float f2 -> Bool (abs_float (f1 -. f2) <= 1e-6)
                      | VectI(n1, l1), VectI(n2, l2) when n1 = n2 -> Bool (l1 = l2)
                      | VectF(n1, l1), VectF(n2, l2) when n1 = n2 -> Bool (List.for_all2 (fun x y -> abs_float (x -. y) <= 1e-6) l1 l2)
                      | MatrI(n1, m1, l1), MatrI(n2, m2, l2) when n1 = n2 && m1 = m2 -> Bool (l1 = l2)
                      | MatrF(n1, m1, l1), MatrF(n2, m2, l2) when n1 = n2 && m1 = m2 -> Bool (
                        List.for_all2 (fun row1 row2 -> 
                          List.for_all2 (fun x y -> abs_float (x -. y) <= 1e-6) row1 row2
                        ) l1 l2
                        )
                      | _, _ -> failwith (sprintf "How did it come through the type-checker to the IsEq function!?" )
                      )
| IsNotEq(e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                      match (t1, t2) with
                      | Bool b1, Bool b2 -> Bool (b1 <> b2)
                      | Int n1, Int n2 -> Bool (n1 <> n2)
                      | Float f1, Float f2 -> Bool (abs_float (f1 -. f2) > 1e-6)
                      | VectI(n1, l1), VectI(n2, l2) when n1 = n2 -> Bool (l1 <> l2)
                      | VectF(n1, l1), VectF(n2, l2) when n1 = n2 -> Bool (List.exists2 (fun x y -> abs_float (x -. y) > 1e-6) l1 l2)
                      | MatrI(n1, m1, l1), MatrI(n2, m2, l2) when n1 = n2 && m1 = m2 -> Bool (l1 <> l2)
                      | MatrF(n1, m1, l1), MatrF(n2, m2, l2) when n1 = n2 && m1 = m2 -> Bool (
                        List.exists2 (fun row1 row2 -> 
                          List.exists2 (fun x y -> abs_float (x -. y) > 1e-6) row1 row2
                        ) l1 l2
                        )
                      | _, _ -> failwith (sprintf "How did it come through the type-checker to the IsNotEq function!?" )
                      )

(* how to precision check here? *)
| IsLt (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                match (t1, t2) with
                | Int n1, Int n2 -> Bool (n1 < n2)
                | Float f1, Float f2 -> Bool (f1 < f2 && abs_float (f1 -. f2) > 1e-6)
                | _, _ -> failwith (sprintf "How did it come through the type-checker to the IsLt function!?" )
                )
| IsLe (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                match (t1, t2) with
                | Int n1, Int n2 -> Bool (n1 <= n2)
                | Float f1, Float f2 -> Bool (f1 < f2 || abs_float (f1 -. f2) <= 1e-6)
                | _, _ -> failwith (sprintf "How did it come through the type-checker to the IsLe function!?" )
                )
| IsGt (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                match (t1, t2) with
                | Int n1, Int n2 -> Bool (n1 > n2)
                | Float f1, Float f2 -> Bool (f1 > f2 && abs_float (f1 -. f2) > 1e-6)
                | _, _ -> failwith (sprintf "How did it come through the type-checker to the IsGt function!?" )
                )
| IsGe (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
                match (t1, t2) with
                | Int n1, Int n2 -> Bool (n1 >= n2)
                | Float f1, Float f2 -> Bool (f1 > f2 || abs_float (f1 -. f2) <= 1e-6)
                | _, _ -> failwith (sprintf "How did it come through the type-checker to the IsGe function!?" )
                )
| Dim e -> let t1 = (eval tableofIDs e) in (
                      match t1 with
                      | VectI(n,l) -> dim l
                      | VectF(n,l) -> dim l
                      | _ -> failwith (sprintf "How did it come through the type-checker to the Dim function!?" )
                    )
| Angle (e1, e2) -> let t1 = (eval tableofIDs e1) and t2 = (eval tableofIDs e2) in (
            match (t1, t2) with
            | VectI(n, l1), VectI(m, l2) when n = m -> Float(angle_int l1 l2)
            | VectF(n, l1), VectF(m, l2) when n = m -> Float(angle_float l1 l2)
            | _ -> failwith (sprintf "How did it come through the type-checker to the Angle function!?" )
            )
| Trans e -> let t1 = (eval tableofIDs e) in (
          match t1 with
          | MatrI(n1, m1, l) -> 
            let transposed = List.map (fun i -> List.map (fun row -> List.nth row i) l) (List.init m1 (fun i -> i)) in
            MatrI(m1, n1, transposed)
          | MatrF(n1, m1, l) -> 
            let transposed = List.map (fun i -> List.map (fun row -> List.nth row i) l) (List.init m1 (fun i -> i)) in
            MatrF(m1, n1, transposed)
          | _ -> failwith (sprintf "How did it come through the type-checker to the Trans function!?" )
        )
| Det e -> let t1 = (eval tableofIDs e) in (
          match t1 with
          | MatrI(n1, n2, l) when n1 = n2 -> 
            let rec determinant matrix =
            match matrix with
            | [[x]] -> x
            | _ -> 
              List.mapi (fun i x -> 
                x * (determinant (List.map (fun row -> List.filteri (fun j _ -> j <> i) row) (List.tl matrix))) * (if i mod 2 = 0 then 1 else -1)
              ) (List.hd matrix)
              |> List.fold_left (+) 0
            in
            Int (determinant l)
          | MatrF(n1, n2, l) when n1=n2 -> 
            let rec determinant matrix =
            match matrix with
            | [[x]] -> x
            | _ -> 
              List.mapi (fun i x -> 
                x *. (determinant (List.map (fun row -> List.filteri (fun j _ -> j <> i) row) (List.tl matrix))) *. (if i mod 2 = 0 then 1.0 else -1.0)
              ) (List.hd matrix)
              |> List.fold_left (+.) 0.0
            in
            Float (determinant l)
          | _ -> failwith (sprintf "How did it come through the type-checker to the Det function!?" )
        )
| ForLoop (e1, e2, e3, e4) -> 
          (let _ = eval tableofIDs e1 in
          let rec loop () =
            match eval tableofIDs e2 with
            | Bool true -> 
              let _ = eval tableofIDs e4 in
              let _ = eval tableofIDs e3 in
              loop ()
            | Bool false -> Unit
            | _ -> failwith (sprintf "How did it come through the type-checker to the ForLoop function!?")
          in
          loop ())
| WhileLoop (e1, e2) ->(
          let rec loop () =
            match eval tableofIDs e1 with
            | Bool true -> 
              let _ = eval tableofIDs e2 in
              loop ()
            | Bool false -> Unit
            | _ -> failwith (sprintf "How did it come through the type-checker to the WhileLoop function!?")
          in
          loop ()
                  )
| IfThenElse (e1, e2, e3) -> (
            match eval tableofIDs e1 with
            | Bool true -> eval tableofIDs e2
            | Bool false -> eval tableofIDs e3
            | _ -> failwith (sprintf "How did it come through the type-checker to the IfThenElse function!?") 
          )
| Seq (e1, e2) -> let _ = (eval tableofIDs e1) and _ = (eval tableofIDs e2) in Unit
| NoOp -> Unit
| Scope(e) -> let _ = eval (int_enter_scope tableofIDs) e in (Unit)

| AssignVectorAtIndex(str, e1, e2) -> 
          let t1 = eval tableofIDs e1 and t2 = eval tableofIDs e2 in 
          (match t1 with 
          | Int n -> (match getvaluID tableofIDs str with
              | VectI(len, l) when n >= 0 && n < len -> 
                let updated_list = List.mapi (fun i x -> if i = n then (match t2 with Int v -> v | _ -> failwith "e2 isn't an integer") else x) l in
                let _ = mutvaluID tableofIDs str (VectI(len, updated_list)) in Unit
              | VectF(len, l) when n >= 0 && n < len -> 
                let updated_list = List.mapi (fun i x -> if i = n then (match t2 with Float v -> v | _ -> failwith "e2 isn't a float") else x) l in
                let _ = mutvaluID tableofIDs str (VectF(len, updated_list)) in Unit
              | _ -> failwith "Invalid vector or index out of bounds")
          | _ -> failwith (sprintf "How did it come through the type-checker to the AssignVectorAtIndex function!?" ))

| AssignMatAtIndex(str, e1, e2, e3) -> 
          let t1 = eval tableofIDs e1 and t2 = eval tableofIDs e2 and t3 = eval tableofIDs e3 in 
          (match (t1, t2) with 
          | Int r, Int c -> (match getvaluID tableofIDs str with
              | MatrI(rows, cols, l) when r >= 0 && r < rows && c >= 0 && c < cols -> 
                let updated_matrix = List.mapi (fun i row -> 
                if i = r then 
                  List.mapi (fun j x -> if j = c then (match t3 with Int v -> v | _ -> failwith "e3 isn't an integer") else x) row 
                else row) l in
                let _ = mutvaluID tableofIDs str (MatrI(rows, cols, updated_matrix)) in Unit
              | MatrF(rows, cols, l) when r >= 0 && r < rows && c >= 0 && c < cols -> 
                let updated_matrix = List.mapi (fun i row -> 
                if i = r then 
                  List.mapi (fun j x -> if j = c then (match t3 with Float v -> v | _ -> failwith "e3 ins't a float") else x) row 
                else row) l in
                let _ = mutvaluID tableofIDs str (MatrF(rows, cols, updated_matrix)) in Unit
              | _ -> failwith "Invalid matrix or index out of bounds")
          | _ -> failwith (sprintf "How did it come through the type-checker to the AssignMatAtIndex function!?" ))

| LookVectAtIndex(str, e) -> 
                  let t1 = eval tableofIDs e in 
                  (match t1 with 
                  | Int n -> 
                    (match getvaluID tableofIDs str with
                    | VectI(len, l) when n >= 0 && n < len -> Int (List.nth l n)
                    | VectF(len, l) when n >= 0 && n < len -> Float (List.nth l n)
                    | _ -> failwith "Invalid vector or index out of bounds")
                  | _ -> failwith (sprintf "How did it come through the type-checker to the LookVectAtIndex function!?" ))

| LookMatAtIndex(str, e1, e2) -> 
                  let t1 = eval tableofIDs e1 and t2 = eval tableofIDs e2 in 
                  (match (t1, t2) with 
                  | Int r, Int c -> 
                    (match getvaluID tableofIDs str with
                    | MatrI(rows, cols, l) when r >= 0 && r < rows && c >= 0 && c < cols -> Int (List.nth (List.nth l (r)) (c))
                    | MatrF(rows, cols, l) when r >= 0 && r < rows && c >= 0 && c < cols -> Float (List.nth (List.nth l (r)) (c))
                    | _ -> failwith (sprintf "Invalid matrix or index out of bounds"))
                  | _ -> failwith (sprintf "How did it come through the type-checker to the LookMatAtIndex function!?"))

| Sqrt(e) -> let t = eval tableofIDs e in (match t with Float f -> Float (sqrt(f)) | _ -> failwith (sprintf "How did it come through the type-checker to the Sqrt function!?"));
| Minor(str, e2, e3) -> 
                    let t1 = eval tableofIDs (LookupIdent str) 
                    and t2 = eval tableofIDs e2 
                    and t3 = eval tableofIDs e3 in 
                    (match (t1, t2, t3) with 
                    | MatrI(n1, n2, l), Int r, Int c -> (
                        if r >= 0 && r < n1 && c >= 0 && c < n2 then (
                          let minor_matrix = 
                            List.mapi (fun i row -> 
                            if i = r then None 
                            else Some (List.mapi (fun j elem -> if j = c then None else Some elem) row 
                                  |> List.filter_map (fun x -> x))
                            ) l |> List.filter_map (fun x -> x)
                          in
                          MatrI(n1 - 1, n2 - 1, minor_matrix))
                        else(
                          failwith (sprintf "Index out of bounds")
                        )
                      )
                    | MatrF(n1, n2, l), Int r, Int c  -> (
                        if r >= 0 && r < n1 && c >= 0 && c < n2 then (
                          let minor_matrix = 
                            List.mapi (fun i row -> 
                            if i = r then None 
                            else Some (List.mapi (fun j elem -> if j = c then None else Some elem) row 
                                  |> List.filter_map (fun x -> x))
                            ) l |> List.filter_map (fun x -> x)
                          in
                          MatrF(n1 - 1, n2 - 1, minor_matrix))
                        else(
                          failwith (sprintf "Index out of bounds")
                        )
                      )
                    | _ -> failwith (sprintf "How did it come through the type-checker to the Minor function!?" )
                    )
| Invert(e) -> let t = eval tableofIDs e in (
            match t with 
            | MatrI(n1, n2, l) when n1 = n2 -> 
                      let determinant = match eval tableofIDs (Det e) with
                        | Int d -> d
                        | _ -> failwith "Unexpected type for determinant"
                      in
                      if determinant = 0 then failwith "Matrix is not invertible (determinant is zero)"
                      else
                        let adjugate = 
                        List.mapi (fun i row -> 
                        List.mapi (fun j _ -> 
                        let minor_matrix = 
                          List.mapi (fun x r -> 
                          if x = i then None 
                          else Some (List.mapi (fun y c -> if y = j then None else Some c) r |> List.filter_map (fun x -> x))
                          ) l |> List.filter_map (fun x -> x)
                        in
                        let minor_det = match eval tableofIDs (Det (ConstIM(n1 - 1, n2 - 1, minor_matrix))) with
                          | Int d -> d
                          | _ -> failwith "Unexpected type for minor determinant"
                        in
                        if (i + j) mod 2 = 0 then minor_det else -minor_det
                        ) row
                        ) l
                        in
                        let transposed = 
                        List.map (fun i -> List.map (fun row -> List.nth row i) adjugate) (List.init n1 (fun i -> i))
                        in
                        MatrI(n1, n2, List.map (fun row -> List.map (fun x -> x / determinant) row) transposed)
            | MatrF(n1, n2, l) when n1 = n2 -> 
                        let determinant = match eval tableofIDs (Det e) with
                          | Float d -> d
                          | _ -> failwith "Unexpected type for determinant"
                        in
                        if abs_float determinant <= epsilon then failwith "Matrix is not invertible (determinant is zero)"
                        else
                          let adjugate = 
                          List.mapi (fun i row -> 
                          List.mapi (fun j _ -> 
                          let minor_matrix = 
                            List.mapi (fun x r -> 
                            if x = i then None 
                            else Some (List.mapi (fun y c -> if y = j then None else Some c) r |> List.filter_map (fun x -> x))
                            ) l |> List.filter_map (fun x -> x)
                          in
                          let minor_det = match eval tableofIDs (Det (ConstFM(n1 - 1, n2 - 1, minor_matrix))) with
                            | Float d -> d
                            | _ -> failwith "Unexpected type for minor determinant"
                          in
                          if (i + j) mod 2 = 0 then minor_det else -.minor_det
                          ) row
                          ) l
                          in
                          let transposed = 
                          List.map (fun i -> List.map (fun row -> List.nth row i) adjugate) (List.init n1 (fun i -> i))
                          in
                          MatrF(n1, n2, List.map (fun row -> List.map (fun x -> x /. determinant) row) transposed)
            | _ -> failwith (sprintf "How did it come through the type-checker to the Minor function!?")
            );
| CIvect(e) -> VectI(e, createint e 0)
| CFvect(e) -> VectF(e, createfloat e 0.0)
| CImatr(e1, e2) -> MatrI(e1, e2, List.init e1 (fun _ -> createint e2 0))
| CFmatr(e1, e2) -> MatrF(e1, e2, List.init e1 (fun _ -> createfloat e2 0.0))
| Exit -> failwith (sprintf "Exited with exit code 1")
;;


let value_to_string = function
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> s
  | VectI(_, l) -> "[" ^ (String.concat ", " (List.map string_of_int l)) ^ "]"
  | VectF(_, l) -> "[" ^ (String.concat ", " (List.map string_of_float l)) ^ "]"
  | MatrI(_, _, l) -> 
      let rows = List.map (fun row -> "[" ^ (String.concat ", " (List.map string_of_int row)) ^ "]") l in
      "[\n" ^ (String.concat "\n" rows) ^ "\n]"
  | MatrF(_, _, l) -> 
      let rows = List.map (fun row -> "[" ^ (String.concat ", " (List.map string_of_float row)) ^ "]") l in
      "[\n" ^ (String.concat "\n" rows) ^ "\n]"
  | Unit -> "Unit"
  ;;
