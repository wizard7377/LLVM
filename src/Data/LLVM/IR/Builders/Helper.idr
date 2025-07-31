module Data.LLVM.IR.Builders.Helper

--import Data.LLVM.Class
import Data.LLVM.IR.Core       
--import Data.LLVM.Write
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Program
import Data.LLVM.IR.Alias
import Data.List
import Data.Walk
import Data.LLVM.IR.Util
import Data.LLVM.IR.Builders.Core
import Data.LLVM.IR.Builders.Ops
import Data.LLVM.IR.Builders.Math
import Data.LLVM.IR.Builders.Control


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
||| Create a high-level switch construct with automatic label generation.
|||
||| Creates a complete switch statement with automatic generation of case labels
||| and proper control flow. This is a higher-level interface than mkSwitch that
||| handles the complexity of label generation and branch construction.
|||
||| @ bloc The block type that can contain the generated statements
||| @ cprefix Prefix for automatically generated labels (defaults to "SWITCH_")
||| @ matching The typed expression to switch on
||| @ cases List of (pattern, statements) pairs for each case
||| @ otherwise Statements to execute if no cases match (default case)
switch : {default "SWITCH_" cprefix : String} -> (matching : WithType LExpr) -> (cases : List (LExpr, List LStatement)) -> (otherwise : List LStatement) -> (Name -> List LStatement)
switch {cprefix} (MkWithType ty e) branches defaultCase target = let 

    indexedBranches = indexed branches
    namedBranches = map (\(branch, idx) => (cprefix ++ show idx, branch)) indexedBranches
    switchInstruction = Operation target $ mkSwitch ty e (local $ cprefix ++ "DEFAULT") (map (\(labels, (pat, _)) => caseBranch ty pat $ LVar $ local labels) namedBranches)
    -- Operation Trash 
    jmpAfter = Operation Trash $ TerminatorOp $ JumpBr (LVar $ local $ cprefix ++ "AFTER")
    -- Branhces
    branchesWithLabels : List _ = concat $ map (\(labels, (pat, stmts)) => ([Labelled labels] ++ stmts ++ [jmpAfter])) namedBranches
    defaultWithLabel = [Labelled (cprefix ++ "DEFAULT")] ++ defaultCase ++ [jmpAfter]
    finally = Labelled $ cprefix ++ "AFTER"
    generated = (switchInstruction :: branchesWithLabels) ++ defaultWithLabel ++ [finally]
    in generated


export
||| Example LLVM module demonstrating the use of builder functions.
|||
||| This example creates a complete LLVM module with data layout, target specification,
||| a global variable, and a simple function definition. It showcases how to use
||| the builder functions to construct LLVM IR programmatically.
exampleModule : LModule
exampleModule = 
  mkModule {
    dataLayout = Just "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128",
    target = Just "x86_64-unknown-linux-gnu",
    text = [
      -- Simple global variable
      GlobalDefC $ globalDef "myGlobal" (LInt 32) {init = Just (LInt 42)},
      
      -- Function definition with custom calling convention
      FunctionDefC $ functionDef "add" {callingConvention = Just C} (LInt 32) 
        [functionArg (LInt 32) {name = Just "a"}, functionArg (LInt 32) {name = Just "b"}]
        (MkFunctionBody [
          label "entry",
          assign (local "result") (add (LInt 32) (localPtr "a") (localPtr "b")),
          ret (LInt 32) (localPtr "result")
        ])
    ]
  }

-- Example of a simple function call
export
||| Example function call operation demonstrating simple function calling.
|||
||| This example creates a function call to an "add" function with two integer
||| arguments, showcasing how to use the function call builders with typed arguments.
exampleCall : LOperation
exampleCall = simpleCall 
  (LFun (LInt 32) [LInt 32, LInt 32]) 
  (globalPtr "add") 
  [withType (LInt 32) (constExpr (LInt 5)), withType (LInt 32) (constExpr (LInt 10))]

export 
||| Create a bytecode specification with main module and additional modules.
|||
||| Creates a bytecode specification that can contain multiple LLVM modules
||| with an optional main module designation. This is used for organizing
||| multi-module LLVM programs.
|||
||| @ mainMod Optional name of the main module
||| @ modules List of (name, module) pairs for all modules in the bytecode
bytecode : {default Nothing mainMod : Maybe String} -> {default [] modules : List (String, LModule)} ->
    Bytecode
bytecode {mainMod} {modules} = MkBytecode mainMod modules

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
    {default [] args : List FunctionArgSpec} ->
    {default LVoid resType : LType} ->
    LClause
foriegnDec name {args} {resType} = FunctionDecC $ functionDec name resType args
