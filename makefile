FRAMEWORKS     = -I/opt/local/include -L/opt/local/lib -ldl -lffi
BUILD_PATH     = ./bin
BUILD_FLAGS    = -std=c99 -Wall -g -O0 -Wno-switch
MAJI_SRC       = ./src/main.c
MAJIVM_SRC     = ./src/bytecode/bytecode_runner.c
BINS           = $(BUILD_PATH)/majic $(BUILD_PATH)/majivm

.PHONY: all clean install

all: clean $(BINS)

install: BUILD_FLAGS=-std=c99 -O2 -Wno-switch
install: clean $(BINS)

clean:
	rm -rf $(BUILD_PATH)

$(BUILD_PATH)/majic: $(MAJI_SRC)
	mkdir -p $(BUILD_PATH)
	clang $^ $(BUILD_FLAGS) -o $@

$(BUILD_PATH)/majivm: $(MAJIVM_SRC)
	mkdir -p $(BUILD_PATH)
	clang $^ $(BUILD_FLAGS) $(FRAMEWORKS) -o $@
