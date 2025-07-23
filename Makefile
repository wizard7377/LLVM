cg ?= chez
pack ?=
opts ?= 

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
	rm -rf build/exec/llvm-test
	idris2 --clean test.ipkg

.PHONY: build install test clean-test