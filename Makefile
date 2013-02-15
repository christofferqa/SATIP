# Directories
SRC_DIR := src
INCL_DIRS := src,src/algorithms,src/analysis/controlFlowAnalysis,src/analysis/typeAnalysis
EXCL_DIRS := lib,tests
BLD_DIR := _build

# Base invokation of menhir
OCAMLMENHIR := menhir

# Base invokation of ocamlbuild
OCAMLBUILD := ocamlbuild -no-links -use-menhir -menhir "${OCAMLMENHIR} -v" -Is $(INCL_DIRS) -Xs $(EXCL_DIRS)

tip.byte:
	@echo "*** Building tip.byte"
	@$(OCAMLBUILD) $(SRC_DIR)/main.byte
	@rm -f parser.conflicts
	@if (test -f $(BLD_DIR)/$(SRC_DIR)/parser.conflicts ); \
          then ln -sf $(BLD_DIR)/$(SRC_DIR)/parser.conflicts parser.conflicts; fi
	@ln -sf $(BLD_DIR)/$(SRC_DIR)/main.byte tip.byte
	@ln -sf tip.byte tip
	@echo "*** Done"

clean:
	@echo "*** Cleaning"
	$(OCAMLBUILD) -clean
	rm -f tip tip.byte
	@echo "*** Done"