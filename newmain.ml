(*

ocamlc -c ast.ml && ocamlyacc parser.mly && ocamlc -c parser.mli parser.ml && ocamllex lexer.mll && ocamlc -c lexer.ml newmain.ml 
ocamlc -o main ./ast.cmo ./parser.cmo ./lexer.cmo ./newmain.cmo
./main -f ./input.txt

with interpreter:
ocamlc -c ast.ml && ocamlyacc parser.mly && ocamlc -c parser.mli parser.ml && ocamllex lexer.mll && ocamlc -c lexer.ml interpreter.ml newmain.ml 
ocamlc -o main ./ast.cmo ./parser.cmo ./lexer.cmo ./interpreter.cmo ./newmain.cmo 
./main -f ./input.txt

**)

open Parser
open Ast

(* Parse the input and print the AST *)
let parse_and_print_ast lexbuf output_channel =
    let tree = Parser.main Lexer.segmentar lexbuf in
    Printf.fprintf output_channel "%s\n" (Ast.ast_to_string tree); flush output_channel; tree

(* Interactive mode: read a line, parse it, and loop *)
let rec interactive_mode output_channel =
  print_string "> "; flush stdout;
  try
    let line = read_line () in
    if String.trim line = "" then interactive_mode output_channel
    else (
      let lexbuf = Lexing.from_string line in
      let _ = parse_and_print_ast lexbuf output_channel; in
      interactive_mode output_channel
    )
  with End_of_file ->
    Printf.fprintf output_channel "EOF\n"; flush output_channel

type symtable = (string, Interpreter.values) Hashtbl.t list;;

let () =
  (* Parse command-line arguments *)
  let file_input = ref false in
  let filename = ref "" in
  
  let speclist = [
    ("-f", Arg.Set file_input, "Read input from a file");
  ] in
  
  let usage_msg = "Usage: testmylex [-f filename]" in
  Arg.parse speclist (fun arg -> filename := arg) usage_msg;
  
  (* Open output file *)
  let output_channel = open_out "./test1_output.txt" in
  
  if !file_input then
    begin
      if !filename = "" then (
         Printf.eprintf "Error: No filename provided with -f flag\n";
         close_out output_channel;
         exit 1
      ) else
        let in_channel = open_in !filename in
        let lexbuf = Lexing.from_channel in_channel in
        let tree = parse_and_print_ast lexbuf output_channel in
        (try
           let _ = Ast.type_of [Hashtbl.create 16] tree in
           let global_scope : symtable = [Hashtbl.create 16] in
           let _ = Interpreter.eval global_scope tree in
           print_endline "Type check passed."
         with Failure msg ->
           Printf.eprintf "Type error: %s\n" msg);
        close_in in_channel;
        close_out output_channel
    end
  else
    interactive_mode output_channel