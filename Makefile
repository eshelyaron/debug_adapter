PROJECT   = debug_adapter
PROLOG    = swipl
SELFDIR   = $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SRC_DIR   = prolog
BIN_DIR   = bin
SERVER    = $(BIN_DIR)/$(PROJECT)
SRC_FILES = $(wildcard $(SRC_DIR)/*.pl)
TEST_DIR  = test

.PHONY: all check clean

all: $(SERVER)

$(SERVER): $(SRC_FILES)
	@mkdir $(BIN_DIR) 2>/dev/null || true
	@$(PROLOG) -o $(SERVER) -c $(SRC_DIR)/main.pl

check:
	@$(PROLOG) -g "findall(T, directory_member($(TEST_DIR), T, [extensions(['plt'])]), Ts), load_files(Ts, [])" -g run_tests -t halt
clean:
	@rm -r $(BIN_DIR) 2>/dev/null || true
