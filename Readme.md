# Linear Algebra DSL

This project implements a **Domain-Specific Language (DSL)** for performing linear algebra operations. The implementation is written in **OCaml**, leveraging `ocamlyacc` and `ocamllex` for parsing and lexical analysis.

---

## 📁 Project Structure
```
linear-algebra-DSL/
├── build/           # Directory for compiled files (generated during build)
│   ├── *.cmo       # Compiled object files
│   ├── *.cmi       # Compiled interface files
│   ├── parser.mli  # Generated parser interface
│   ├── parser.ml   # Generated parser implementation
│   ├── lexer.ml    # Generated lexer implementation
├── ast.ml          # Abstract Syntax Tree definitions
├── parser.mly      # Grammar rules for the parser
├── lexer.mll       # Lexer definitions
├── interpreter.ml  # Interpreter for evaluating the DSL
├── newmain.ml      # Main entry point for the program
├── input.txt       # Sample input file for testing
├── output.txt      # Output file for viewing AST
├── main            # Final executable (generated after build)
├── Makefile        # Build automation script
└── Readme.md       # Project documentation
```

---

## ⚙️ How to Build and Run

### 🔹 Build the Project
Run the following command in the terminal:
```bash
make
```
This will:
- Compile all `.ml` files into `.cmo` and `.cmi` files.
- Generate `parser.ml`, `parser.mli`, and `lexer.ml` in the `build/` directory.
- Create the final executable `main` in the root directory.

### 🔹 Run the Program
After building, you can execute the program with:
```bash
./main -f input.txt
```
The `-f` flag specifies that the program should read input from the file `input.txt`.

### 🔹 Clean the Build
To remove all generated files and clean the project directory, run:
```bash
make clean
```

---

## 📌 Notes
- The `build/` directory stores all intermediate files (`.cmo`, `.cmi`, `parser.ml`, `parser.mli`, `lexer.ml`) to keep the project organized.
- The final executable `main` is located in the root directory for easy access.
- Modify `input.txt` to test the program with different inputs.

---

## 📦 Dependencies
Ensure you have the following tools installed:
- **OCaml**
- **ocamlyacc** (for parser generation)
- **ocamllex** (for lexical analysis)

---

## 📜 License
This project is for **educational purposes** and is **not licensed for commercial use**.

---