FRAMEWORKS     =
BUILD_PATH     = ./bin
BUILD_FLAGS    = -std=c99 -Wall -g -O0 -Wno-switch
MAJI_SRC       = ./src/main.c
BINS           = $(BUILD_PATH)/majic

.PHONY: all clean install

all: clean $(BINS)

install: BUILD_FLAGS=-std=c99 -O2
install: clean $(BINS)

clean:
	rm -rf $(BUILD_PATH)

$(BUILD_PATH)/majic: $(MAJI_SRC)
	mkdir -p $(BUILD_PATH)
	clang $^ $(BUILD_FLAGS) $(FRAMEWORKS) -o $@
