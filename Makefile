# TODO: Clean up 
# TODO: Cleaning work with new structure
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
ffiFiles := $(wildcard llvm-ffi/llvm-ffi/*.idr)
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
	@$(CC) -c -fPIC support/array.c -o support/array.o 
	@$(CC) -o $@ -shared support/array.o 
build_ffi: array.so $(ffiFiles) check-llvm
	$(IDRIS) $(OPTS) --build llvm-ffi/llvm-ffi.ipkg

install_ffi: array.so $(ffiFiles) check-llvm build_ffi
	$(IDRIS) $(OPTS) --install llvm-ffi/llvm-ffi.ipkg


build: array.so $(srcFiles) check-llvm install_ffi
	$(IDRIS) $(OPTS) --build llvm-idr/llvm-idr.ipkg

install: array.so $(srcFiles) check-llvm install_ffi
	$(IDRIS) $(OPTS) --install llvm-idr/llvm-idr.ipkg

test: clean-test build 
	@echo "Building and running LLVM tests..."
	$(IDRIS) $(OPTS) --build llvm-test/llvm-test.ipkg
	@echo "Running test executable..."
	@$(DBG) ./llvm-test/out/llvm-test

clean: clean-test
	@echo "Cleaning build artifacts..."
	@rm -rf build
	@mkdir -p build
	@rm -rf generated
	@rm -rf docs  
	@mkdir -p generated
	@mkdir -p docs
	@$(IDRIS) $(OPTS) --clean llvm-test/llvm-test.ipkg
	@$(IDRIS) $(OPTS) --clean llvm-idr/llvm-idr.ipkg
	@rm -f support/array.so 
	@rm -f support/array.o
	@rm -f array.so
	@$(PACK) clean llvm-idr/llvm-idr.ipkg
clean-test:
	@echo "Cleaning test build artifacts..."
	@rm -rf generated 
	@mkdir -p generated
	@mkdir -p generated/llvm
	@mkdir -p generated/temp

docs: install
	$(IDRIS) $(OPTS) --mkdoc llvm-idr/llvm-idr.ipkg
	@cp -r build/docs/. docs
.PHONY: build install test clean-test clean 

runTests: $(testFiles)
generated/%.ss: generated/%.ll
	llvm-as -o $@ $<

repl: install 
	@echo "Starting $(IDRIS) REPL with LLVM package..."
	$(REPL) $(IDRIS) $(OPTS) --repl llvm-idr/llvm-idr.ipkg



holes: $(srcFiles)
	@rg --regexp='\?[_\w]+' --context=1 --glob '*.idr'
	@echo ""
	@echo "Number of holes"
	@echo ""
	@rg --regexp='\?[_\w]+' --context=1 --glob '*.idr' --count

getSrc: 
	@echo "Getting all source files"
	@python ./scripts/getsrc.py