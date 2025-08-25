||| LLVM IR representation and manipulation library.
|||
||| This is the main module for the LLVM IR library, providing a complete
||| Idris interface for creating, manipulating, and serializing LLVM IR.
||| 
||| The library includes:
||| - Core data types for LLVM IR constructs (LCore)
||| - Type classes for encoding IR to text (LClass) 
||| - Serialization to LLVM IR text format (LWrite)
||| - Instruction and operation types (LOps)
||| - Program structure and top-level definitions (LProgram)
||| - Builder functions and DSL interfaces (Interface)
||| - Type conversions and casting utilities (LCasts)
||| - Optimization pass integration (LPass)
||| - Foreign function interface utilities (LForeign)
|||
||| Example usage:
||| ```idris
||| import Data.LLVM
||| 
||| myModule : LModule
||| myModule = MkLModule {
|||   text = [
|||     GlobalDefC $ globalDef "myGlobal" (Core.LInt 32) {init = Just (Core.LInt 42)},
|||     FunctionDefC $ functionDef "main" LVoid [] (MkBasicBlock [
|||       ret LVoid
|||     ])
|||   ]
||| }
||| ```
module Data.LLVM 

import public Data.LLVM.Class as LClass
import public Data.LLVM.Write.Text.Encode as LWrite
import public Data.LLVM.IR as LIR
import public Data.LLVM.Builders
import public Data.LLVM.IR.Alias
import public Data.LLVM.Interface
import public Data.LLVM.Casts as LCasts
import public Data.LLVM.Pass as LPass
import public Data.LLVM.Foreign as LForeign
import public Data.LLVM.CC as CC