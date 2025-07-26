cg ?= chez
pack ?=
opts ?= 
testFiles := $(patsubst %.ll,%.ss,$(wildcard generated/*.ll)) 
build: 
	idris2 --build llvm.ipkg

install: build
	idris2 --install llvm.ipkg

test: install
	@echo "Building and running LLVM tests..."
	idris2 --build test.ipkg
	@echo "Running test executable..."
	./build/exec/llvm-test

clean-test:
	@echo "Cleaning test build artifacts..."
	rm -rf generated 
	mkdir -p generated
	idris2 --clean test.ipkg

docs: install
	idris2 --mkdoc llvm.ipkg
.PHONY: build install test clean-test

runTests: $(testFiles)
generated/%.ss: generated/%.ll
	llvm-as -o $@ $<