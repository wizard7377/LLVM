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
SUPPORT_DIR := llvm-ffi/support
export IDRIS_LLVM_VERBOSITY := $(VERB)
ifeq ($(loud), 0)
  OPTS += --quiet
endif
ifeq ($(loud), 2)
  OPTS += --verbose
endif 

IDRIS ?= idris2