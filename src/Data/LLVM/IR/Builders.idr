||| LLVM IR builder functions for constructing LLVM IR programmatically.
|||
||| This module provides convenient builder functions for creating LLVM IR constructs
||| with sensible defaults and optional parameters. The builders cover:
|||
||| - **Terminator operations**: switch, invoke, unreachable, etc.
||| - **Arithmetic operations**: add, sub, mul, div, rem with overflow/exact flags
||| - **Bitwise operations**: and, or, xor, shift operations
||| - **Comparison operations**: icmp, fcmp with different predicates
||| - **Memory operations**: alloca, load, store with various flags
||| - **Type conversion**: trunc, zext, sext, bitcast, fp conversions
||| - **Vector operations**: insert/extract/shuffle elements
||| - **Aggregate operations**: insert/extract values from structs/arrays
||| - **Control flow**: phi nodes, select, freeze
||| - **Type shortcuts**: i1, i8, i16, i32, i64, ptr, float, double
||| - **Constant builders**: integers, booleans, strings, arrays, etc.
|||
||| All builder functions use default arguments where sensible to reduce verbosity
||| while still allowing full customization when needed.
module Data.LLVM.IR.Builders

import public Data.LLVM.IR.Builders.Core
import public Data.LLVM.IR.Builders.Ops
import public Data.LLVM.IR.Builders.Math
import public Data.LLVM.IR.Builders.Control
import public Data.LLVM.IR.Builders.Helper