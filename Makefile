PROJECT = debug_adapter

PROLOG = swipl

SELFDIR = $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SRC_DIR = prolog
BIN_DIR = bin
BIN_OUT = $(BIN_DIR)/$(PROJECT)
DOC_DIR = doc

VERSION = $(shell sed -n "s/version('\(.*\)')./\1/p" $(SELFDIR)/pack.pl)
NEXTVER = $(shell awk -F. '{ print $$1 "." $$2 "." $$3+1 }' <<< $(VERSION))

SRC_FILES = $(wildcard $(SRC_DIR)/*.pl)

.PHONY: all clean install check doc bump-patch

all: $(BIN_OUT)

$(BIN_OUT): $(SRC_FILES)
	@mkdir $(BIN_DIR) 2>/dev/null || true
	@$(PROLOG) -o $(BIN_OUT) -c $(SRC_DIR)/main.pl -O

clean:
	@rm -r $(BIN_DIR) $(DOC_DIR) 2>/dev/null || true

doc:
	@swipl -g "['$(SRC_DIR)/main.pl'], doc_save('.', [recursive(true)])" -g halt

bump-patch:
	@sed "s/version('.*')./version('$(NEXTVER)')./g" $(SELFDIR)/pack.pl > $(SELFDIR)/pack.pl.tmp && mv $(SELFDIR)/pack.pl.tmp $(SELFDIR)/pack.pl

version:
	@echo $(VERSION)

next-version:
	@echo $(NEXTVER)
