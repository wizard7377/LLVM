cg ?= chez
pack ?=
opts ?= 
testFiles := $(patsubst %.ll,%.ss,$(wildcard generated/*.ll)) 

build: 
	idris2 --build llvm.ipkg

install: build
	idris2 --install llvm.ipkg

test: install clean-test
	@echo "Building and running LLVM tests..."
	idris2 --build test.ipkg
	@echo "Running test executable..."
	./build/exec/llvm-test

clean: clean-test
	@echo "Cleaning build artifacts..."
	rm -rf build
	mkdir -p build
	rm -rf generated
	mkdir -p generated
	idris2 --clean test.ipkg
	idris2 --clean llvm.ipkg
	pack clean llvm.ipkg
clean-test:
	@echo "Cleaning test build artifacts..."
	rm -rf generated 
	mkdir -p generated
	idris2 --clean test.ipkg

docs: install
	idris2 --mkdoc llvm.ipkg
	@cp -r build/docs docs
.PHONY: build install test clean-test clean 

runTests: $(testFiles)
generated/%.ss: generated/%.ll
	llvm-as -o $@ $<
