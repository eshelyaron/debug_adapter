PROJECT   = debug_adapter
PROLOG    = swipl
SELFDIR   = $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SRC_DIR   = prolog
BIN_DIR   = bin
SERVER    = $(BIN_DIR)/$(PROJECT)
SRC_FILES = $(wildcard $(SRC_DIR)/*.pl)

.PHONY: all clean

all: $(SERVER)

$(SERVER): $(SRC_FILES)
	@mkdir $(BIN_DIR) 2>/dev/null || true
	@$(PROLOG) -o $(SERVER) -c $(SRC_DIR)/main.pl

clean:
	@rm -r $(BIN_DIR) 2>/dev/null || true
