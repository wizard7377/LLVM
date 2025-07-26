module System.LLVM.Common 
import System
import public Control.Monad.Either as Either 
import Control.Monad.Either 
public export
data CompilationError : Type where 
    LinkError : String -> CompilationError
    AssembleError : String -> CompilationError
    CompileError : String -> CompilationError
    RunError : String -> CompilationError
    OtherError : String -> CompilationError
export
Show CompilationError where 
    show (LinkError msg) = "Link Error: " ++ msg
    show (AssembleError msg) = "Assemble Error: " ++ msg
    show (CompileError msg) = "Compile Error: " ++ msg
    show (RunError msg) = "Run Error: " ++ msg
    show (OtherError msg) = "Other Error: " ++ msg
public export 
Compile : Type -> Type
Compile a = EitherT CompilationError IO a
currentVerbosity : Int
currentVerbosity = 100
public export
record Context where 
    constructor MkContext
    mainModule : Maybe String
    buildDir : String 
    tempDir : String 
    output : String 


export 
context : {default Nothing mainModule : Maybe String} -> (buildDir : String) -> {default buildDir tempDir : String} -> {default "main" output : String} -> Context
context {mainModule} buildDir {tempDir} {output} = 
    MkContext mainModule buildDir tempDir output

export 
runCmd : {default 10 verb : Int} -> String -> Compile (String, Int)
runCmd {verb} cmd = do
    _ <- (when $ verb < currentVerbosity) (putStrLn $ "Running command: " ++ cmd)
    (sout, res) <- run cmd
    pure (sout, res)


