||| System interface for LLVM compilation and execution.
|||
||| This module provides the main system interface for working with LLVM,
||| including compilation, optimization, linking, and execution of LLVM IR.
||| It re-exports all the necessary components for a complete LLVM workflow.
module System.LLVM

import public System.LLVM.Common
import public System.LLVM.Assembler
import public System.LLVM.Optimize
import public System.LLVM.Compile
import public System.LLVM.Run
import public System.LLVM.Link
import public System.LLVM.Build
import public System.LLVM.Foreign