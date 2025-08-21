
cg ?= chez
ifeq ($(cg), chez) 
	CG_D ?= --inc
else 
	CG_D ?= --cg 
endif
PACK ?= pack
testFiles := $(patsubst %.ll,%.ss,$(wildcard generated/*.ll)) 
loud ?= 0
srcFiles := $(wildcard *.idr)
OPTS += $(CG_D) $(cg)
DBG ?=
VERB ?=
TS ?= tree-sitter 
ASTGREP ?= ast-grep
export IDRIS_LLVM_VERBOSITY := $(VERB)
ifeq ($(loud), 0)
  OPTS += --quiet
endif
ifeq ($(loud), 2)
  OPTS += --verbose
endif 

IDRIS ?= idris2
check-llvm: 
	@echo "Checking LLVM installation..."
	@llvm-config --version
array.so : support/array.c support/array.h 
	$(CC) -c -fPIC support/array.c -o support/array.o 
	$(CC) -o $@ -shared support/array.o 
build: array.so $(srcFiles) check-llvm
	$(IDRIS) $(OPTS) --build llvm.ipkg

install: array.so $(srcFiles) check-llvm
	$(IDRIS) $(OPTS) --install llvm.ipkg

test: install
	@echo "Building and running LLVM tests..."
	$(IDRIS) --build test.ipkg
	@echo "Running test executable..."
	$(DBG) ./build/exec/llvm-test

clean: clean-test
	@echo "Cleaning build artifacts..."
	@rm -rf build
	@mkdir -p build
	@rm -rf generated
	@rm -rf docs  
	@mkdir -p generated
	@mkdir -p docs
	@$(IDRIS) $(OPTS) --clean test.ipkg
	@$(IDRIS) $(OPTS) --clean llvm.ipkg
	@rm -f support/array.so 
	@rm -f support/array.o
	@rm -f array.so
	@$(PACK) clean llvm.ipkg
clean-test:
	@echo "Cleaning test build artifacts..."
	rm -rf generated 
	mkdir -p generated

docs: install
	$(IDRIS) $(OPTS) --mkdoc llvm.ipkg
	@cp -r build/docs/. docs
.PHONY: build install test clean-test clean 

runTests: $(testFiles)
generated/%.ss: generated/%.ll
	llvm-as -o $@ $<

repl: install 
	@echo "Starting $(IDRIS) REPL with LLVM package..."
	$(REPL) $(IDRIS) $(OPTS) --repl llvm.ipkg




