module Data.LLVM.Builders.Helper


--import Data.LLVM.Class
import Data.LLVM.IR.Core       
import Language.Reflection
--import Data.LLVM.Write.Text.Encode
import Data.LLVM.IR.Core
import Data.LLVM.IR.Program
import Data.LLVM.IR.Alias
import Data.List
import Data.Walk
import Data.LLVM.IR.Util
import Data.LLVM.Builders.Core
import Data.LLVM.Builders.Ops
import Data.LLVM.Builders.Math
import Data.LLVM.Builders.Control
import Data.Table
import public Data.Buffer

public export 
lowerTypeReflect : (a : Type) -> LType 
lowerTypeReflect a = case a of 
  Int => i32 
  Double => double 
  String => ptr
  Bool => i1
  Int8 => i8 
  Int16 => i16
  Int32 => i32 
  Int64 => i64
  _ => ?ltr -- FIXME: Find a better way to support this
||| Helper function to add indices to a list.
|||
||| Creates a list of pairs where each element is paired with its
||| index position starting from 0. Used internally for generating
||| unique labels in control flow constructs.
|||
||| @ xs The list to index
indexed : List a -> List (a , Nat)
indexed xs = zip xs [0 .. length xs]

export 
||| Create a bytecode specification with main module and additional modules.
|||
||| Creates a bytecode specification that can contain multiple LLVM modules
||| with an optional main module designation. This is used for organizing
||| multi-module LLVM programs.
|||
||| @ mainMod Optional name of the main module
||| @ modules List of (name, module) pairs for all modules in the bytecode
bytecode : {default Nothing mainMod : Maybe String} -> (modules : Table LModule) -> {default [] bitcode : Table Buffer} -> Bytecode
bytecode {mainMod} modules {bitcode} = MkBytecode mainMod modules bitcode

export 
||| Create a foreign function declaration clause.
|||
||| Creates a function declaration clause for external/foreign functions
||| that are defined outside the current module. This is commonly used
||| for library functions and system calls.
|||
||| @ name The name of the foreign function
||| @ args List of function argument specifications (defaults to empty)
||| @ resType The return type of the function (defaults to void)
foriegnDec : 
    (name : String) ->
    {default [] args : List Argument} ->
    {default LVoid resType : LType} ->
    LClause
foriegnDec name {args} {resType} = functionDec name resType args

