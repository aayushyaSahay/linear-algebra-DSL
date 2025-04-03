BUILD_DIR = build
OCAMLC = ocamlc
OCAMLFLAGS = -I $(BUILD_DIR) -c

all: main

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/ast.cmo: ast.ml | $(BUILD_DIR)
	@$(OCAMLC) $(OCAMLFLAGS) ast.ml -o $@

$(BUILD_DIR)/parser.ml $(BUILD_DIR)/parser.mli: parser.mly | $(BUILD_DIR)
	@ocamlyacc -b $(BUILD_DIR)/parser parser.mly

$(BUILD_DIR)/parser.cmo: $(BUILD_DIR)/parser.ml $(BUILD_DIR)/parser.mli $(BUILD_DIR)/ast.cmo | $(BUILD_DIR)
	@$(OCAMLC) $(OCAMLFLAGS) $(BUILD_DIR)/parser.mli -o $(BUILD_DIR)/parser.cmi
	@$(OCAMLC) $(OCAMLFLAGS) $(BUILD_DIR)/parser.ml -o $@

$(BUILD_DIR)/lexer.ml: lexer.mll | $(BUILD_DIR)
	@ocamllex -o $(BUILD_DIR)/lexer.ml lexer.mll

$(BUILD_DIR)/lexer.cmo: $(BUILD_DIR)/lexer.ml $(BUILD_DIR)/parser.cmo | $(BUILD_DIR)
	@$(OCAMLC) $(OCAMLFLAGS) $(BUILD_DIR)/lexer.ml -o $@

$(BUILD_DIR)/interpreter.cmo: interpreter.ml $(BUILD_DIR)/ast.cmo | $(BUILD_DIR)
	@$(OCAMLC) $(OCAMLFLAGS) interpreter.ml -o $@

$(BUILD_DIR)/newmain.cmo: newmain.ml $(BUILD_DIR)/ast.cmo | $(BUILD_DIR)
	@$(OCAMLC) $(OCAMLFLAGS) newmain.ml -o $@

main: $(BUILD_DIR)/ast.cmo $(BUILD_DIR)/parser.cmo $(BUILD_DIR)/lexer.cmo $(BUILD_DIR)/interpreter.cmo $(BUILD_DIR)/newmain.cmo
	@$(OCAMLC) -o main -I $(BUILD_DIR) $(BUILD_DIR)/ast.cmo $(BUILD_DIR)/parser.cmo $(BUILD_DIR)/lexer.cmo $(BUILD_DIR)/interpreter.cmo $(BUILD_DIR)/newmain.cmo

clean:
	@rm -rf $(BUILD_DIR) main