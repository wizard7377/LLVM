# LLVM IR Library for Idris2 - AI Agent Guide

## Overview
This is an Idris2 library providing complete LLVM IR functionality through a type-safe functional interface. The library enables creating, manipulating, optimizing, and executing LLVM IR programs from within Idris2.

## Architecture

### Core Modules
- **`Data.LLVM`**: IR representation, builders, and serialization
  - `Data.LLVM.IR.Core`: Fundamental LLVM types and values
  - `Data.LLVM.Ops.*`: DSL for constructing IR programmatically
  - `Data.LLVM.Write.*`: Text encoding and serialization to LLVM IR
  - `System.FFI.LLVM.*`: C FFI integration and low-level primitives
- **`System.LLVM`**: Compilation pipeline and system interface
  - `System.LLVM.Compile`: IR to bytecode compilation
  - `System.LLVM.Optimize`: Pass manager integration 
  - `System.LLVM.Link`: Linking multiple modules
  - `System.LLVM.Run`: Execution of compiled programs

### DSL Syntax Conventions (Data.LLVM.Ops.Sugar)
The library provides extensive syntactic sugar with specific operator meanings:
- **`?`** prefix: Creates variables (`?^ "var"` for variable reference)
- **`#`** prefix: Creates constant values (`## 42` for integer literal, `#^ "label"` for label reference)
- **`$`** infix: Creates statements (`"name" <<- instruction`)
- **`:`** infix: Creates types (`:# 32` for i32 type, `4 :<> (:# 32)` for vector type)
- **`^`** suffix: Creates metadata/annotations
- **`&`** prefix: Creates modifiers
- **`<::`** and `>::`**: Type/value wrapping operators

### Key Data Types
- `LModule`: Top-level LLVM module containing globals and functions
- `LFunction`: Function definitions with basic blocks
- `LValue t`: Typed values (constants, variables, instructions)
- `LType`: LLVM type system (integers, floats, pointers, structures)
- `LInstruction`: LLVM instructions (arithmetic, memory, control flow)

### Compilation Context Pattern
Uses `Context` record for compilation configuration:
```idris
record Context where
  constructor MkContext
  passes : List Pass           -- Optimization passes to apply
  mainModule : Maybe String    -- Main module file path
  buildDir : String           -- Build directory
  tempDir : String            -- Temporary directory
  extraIr : List (String, String)  -- Extra IR files
  extraBc : List String       -- Extra bitcode files
  extraObj : List String      -- Extra object files  
  output : String             -- Output file name
```

## Common Patterns

### Example: Basic Function with Instructions
```idris
-- From test suite - demonstrates typical instruction patterns
functionWithInstructions = MkFunction {
  name = "test_function",
  symbolInfo = MkSymbolInfo (Just External) Nothing (Just Default) Nothing,
  returnType = (:# 32),
  args = [MkArgument (:# 32) [] (Just "x"), MkArgument (:# 32) [] (Just "y")],
  body = [
    MkPair "entry" $ MkBasicBlock [
      "sum" <<- ((Add NoWrap) (:# 32) (?^ "x") (?^ "y")),
      "cmp" <<- ((ICmp False) CEq (:# 32) (?^ "sum") ( (## 42)))
    ] (Ret (:# 32) (?^ "sum"))
  ]
}
```

### Example: Vector Operations
```idris
-- Vector type construction and operations
vectorExample = [
  "vec" <<- (InsertElement ((4 :<> (:# 32)) <::> undef) ((:# 32) <::> ( (## 1))) ((:# 32) <::> ( (## 0)))),
  "elem" <<- (ExtractElement ((4 :<> (:# 32)) <::> (?^ "vec")) ((:# 32) <::> ( (## 2))))
]
```

### Example: Testing Pattern  
```idris
-- Standard test function pattern from Test.Helper
encodeFCMTest' : String -> LModule -> IO ()
debugCompile : String -> LModule -> IO ()
```

## Development Workflow

### Build Commands
```bash
# Check dependencies
make check-llvm

# Build library
make build

# Install for use in other projects  
make install

# Run comprehensive test suite
make test

# Interactive development
make repl

# Generate documentation
make docs

# Clean all artifacts
make clean
```

### Package Structure
- `llvm.ipkg`: Main library package with 75+ modules
- `test.ipkg`: Test suite depending on main library + contrib
- Source in `src/`, tests in `test/`, C support in `support/`

### Testing Patterns
Tests use systematic approach with `Test.Helper` providing utilities:
- `encodeFCMTest'`: Test IR encoding to text format
- `debugCompile`: Test compilation with debug output  
- Coverage areas: arithmetic (`(ICmp False)`, `(Add NoWrap)`), control flow (`CondBr`, `Switch`), vectors (`ExtractElement`, `InsertElement`), atomics, metadata
- Test modules in `test/Test/Groups/Module.idr` show comprehensive LLVM feature usage

## Integration Points

### LLVM Dependencies
- Requires LLVM installation (`llvm-config`, `opt`, `llc`, `lli`)
- C FFI through `System.FFI.LLVM.Prim` with 100+ LLVM-C API bindings
- Native array support via `support/array.c` shared library

### Pass Integration
- Wraps LLVM optimization passes (`passes.txt` lists 400+ available passes)
- Context-based pass management in `System.LLVM.Common`
- Custom pass chains via `List Pass` configuration

## Project-Specific Patterns

### Error Handling
Uses `EitherT CompilationError IO` monad for system operations:
```idris
Compile : Type -> Type
Compile a = EitherT CompilationError IO a
```

### Builder Pattern
Extensive use of builder combinators for type-safe IR construction:
```idris
myFunction = functionDef "main" LVoid [] $ MkBasicBlock [
  "result" <<- ((Add NoWrap) (:# 32) (?^ "x") (?^ "y")),
  ret (:# 32) (?^ "result")
]
```

### Foreign C API Integration
- All LLVM-C API functions wrapped in `System.FFI.LLVM.Prim` (100+ bindings)
- Uses `EitherT CompilationError IO` monad for safe foreign calls
- Custom array support via `support/array.c` for C interop

### Module Organization
- Hierarchical module structure mirrors LLVM concepts
- Heavy use of re-exports for clean public interfaces  
- Type classes for uniform encoding/serialization

## Development Notes

### Known Differences from LLVM C++ API
- All statements must have explicit destination targets (may use trash registers)
- More flexible value system using names rather than strict global variable classes
- Type-safe approach prevents many runtime errors common in C++ LLVM API

### Active Development Areas (org/todo.org)
- Foreign C API safety improvements needed
- Builder monad enhancements in progress
- Symbol info support being added
- Documentation improvements ongoing

Focus on the builder DSL patterns and the `System.LLVM` compilation pipeline when working with this codebase. The extensive test suite in `test/Test/Groups/` provides excellent examples of proper usage patterns.
