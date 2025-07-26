module System.LLVM.Build 


import System.LLVM.Common
import Data.LLVM.Program
import System
import System.LLVM.Compile
import System.LLVM.Run
import System.LLVM.Link
import System.LLVM.Assemble
export 
binary : {auto context : Context} -> String -> Compile String
binary {context} input = do
    let cmd = "ld -o " <+> (context.buildDir <+> input) <+> " " <+> (context.buildDir <+> input <+> ".o")
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ CompileError out)
    pure input

export
compile' : {auto context : Context} -> Bytecode -> Compile String
compile' {context} input = do 
    _ <- runCmd $ "mkdir -p " <+> (context.tempDir)
    _ <- runCmd $ "mkdir -p " <+> (context.buildDir)
    let inputs : List _ = input.modules
    outputAsm : List String <-  traverse (\(out,mod) => assembleLLVM {context} mod out) inputs
    linkedBit : String <- linkLLVM {context} outputAsm
    compiled <- compileLLVM {context} linkedBit
    result <- binary {context} compiled
    pure result
export
exec' : {auto context : Context} -> Bytecode -> Compile String
exec' {context} input = do 
    let inputs : List _ = input.modules
    outputAsm : List String <-  traverse (\(out,mod) => assembleLLVM {context} mod out) inputs
    linkedBit : String <- linkLLVM {context} outputAsm
    result <- runLLVM {context} linkedBit
    pure result

export
compile : {auto context : Context} -> Bytecode -> IO (Either CompilationError String)
compile = Either.runEitherT . compile'
export 
exec : {auto context : Context} -> Bytecode -> IO (Either CompilationError String)
exec = Either.runEitherT . exec'