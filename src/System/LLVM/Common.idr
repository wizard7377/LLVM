module System.LLVM.Common 
import System
import public Control.Monad.Either as Either 
import public Data.LLVM.Pass
import Control.Monad.Either 
public export
data CompilationError : Type where 
    LinkError : String -> CompilationError
    AssembleError : String -> CompilationError
    CompileError : String -> CompilationError
    RunError : String -> CompilationError
    OptimizeError : String -> CompilationError
    OtherError : String -> CompilationError
export
Show CompilationError where 
    show (LinkError msg) = "Link Error: " ++ msg
    show (AssembleError msg) = "Assemble Error: " ++ msg
    show (CompileError msg) = "Compile Error: " ++ msg
    show (RunError msg) = "Run Error: " ++ msg
    show (OptimizeError msg) = "Optimize Error: " ++ msg
    show (OtherError msg) = "Other Error: " ++ msg
public export 
Compile : Type -> Type
Compile a = EitherT CompilationError IO a
currentVerbosity : Int
currentVerbosity = 1
public export
record Context where 
    constructor MkContext
    passes : List Pass
    mainModule : Maybe String
    buildDir : String 
    tempDir : String 
    extraIr : List (String, String) 
    extraBc : List String
    extraObj : List String
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