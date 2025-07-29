||| Common types and utilities for LLVM system operations.
|||
||| This module defines the fundamental types and utilities used throughout
||| the LLVM system interface, including error handling, compilation context,
||| and common operations for running external LLVM tools.
module System.LLVM.Common 
import System
import public Control.Monad.Either as Either 
import public Data.LLVM
import Control.Monad.Either 
||| Compilation error types for LLVM operations.
|||
||| Represents various errors that can occur during LLVM compilation,
||| optimization, linking, and execution operations.
public export
data CompilationError : Type where 
    ||| Error during linking phase
    LinkError : String -> CompilationError
    ||| Error during assembly phase
    AssembleError : String -> CompilationError
    ||| Error during compilation phase  
    CompileError : String -> CompilationError
    ||| Error during execution phase
    RunError : String -> CompilationError
    ||| Error during optimization phase
    OptimizeError : String -> CompilationError
    ||| General error not fitting other categories
    OtherError : String -> CompilationError
||| Show instance for compilation errors
export
Show CompilationError where 
    show (LinkError msg) = "Link Error: " ++ msg
    show (AssembleError msg) = "Assemble Error: " ++ msg
    show (CompileError msg) = "Compile Error: " ++ msg
    show (RunError msg) = "Run Error: " ++ msg
    show (OptimizeError msg) = "Optimize Error: " ++ msg
    show (OtherError msg) = "Other Error: " ++ msg

||| Compilation monad for error handling
|||
||| A monad that combines IO with error handling for compilation operations.
||| All LLVM system operations return results in this monad.
public export 
Compile : Type -> Type
Compile a = EitherT CompilationError IO a

||| Current verbosity level for compilation output
|||
||| Controls how much diagnostic information is printed during compilation.
||| Lower values mean more verbose output.
currentVerbosity : Int
currentVerbosity = 1

||| Compilation context containing configuration and state.
|||
||| Holds all the information needed for a complete LLVM compilation pipeline,
||| including optimization passes, modules, and file locations.
public export
record Context where 
    constructor MkContext
    ||| List of optimization passes to apply
    passes : List Pass
    ||| Optional main module file path
    mainModule : Maybe String
    ||| Build directory for intermediate files
    buildDir : String 
    ||| Temporary directory for working files
    tempDir : String 
    ||| Extra LLVM IR files (name, content pairs)
    extraIr : List (String, String) 
    ||| Extra bitcode file paths
    extraBc : List String
    ||| Extra object file paths
    extraObj : List String
    ||| Output file path
    output : String 


export 
context : 
    {default [Level 2] passes : List Pass} -> 
    {default Nothing mainModule : Maybe String} -> 
    (buildDir : String) -> 
    {default buildDir tempDir : String} -> 
    {default [] extraIr : List (String, String)} -> 
    {default [] extraBc : List String} -> 
    {default [] extraObj : List String} -> 
    {default "main" output : String} -> 
    Context
context {passes} {mainModule} buildDir {tempDir} {extraIr} {extraBc} {extraObj} {output} = 
    MkContext passes mainModule buildDir tempDir extraIr extraBc extraObj output

export 
runCmd : {default 10 verb : Int} -> String -> Compile (String, Int)
runCmd {verb} cmd = do
    _ <- (when $ verb < currentVerbosity) (putStrLn $ "Running command: " ++ cmd)
    (sout, res) <- run cmd
    pure (sout, res)


export 
showMsg : {default 10 verb : Int} -> String -> Compile ()
showMsg {verb} msg = do
    _ <- (when $ verb < currentVerbosity) (putStrLn msg)
    pure ()