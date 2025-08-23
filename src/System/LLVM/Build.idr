module System.LLVM.Build 


import System.LLVM.Common
import Data.LLVM
import System
import System.LLVM.Compile
import System.LLVM.Run
import System.LLVM.Link
import System.LLVM.Assembler
import System.LLVM.Optimize
intercalate : String -> List String -> String
intercalate sep [] = ""
intercalate sep [x] = x
intercalate sep (x :: xs) = x ++ foldl (\acc => \y => acc ++ sep ++ y) "" xs
export 
binary : {auto context : Context} -> String -> Compile String
binary {context} input = do
    let cmd = "ld -o " <+> (context.buildDir <+> input) <+> " " <+> (context.buildDir <+> input <+> ".o") <+> " " <+> (intercalate " " context.extraObj)
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ CompileError out)
    pure input

changeOutput : (context : Context) -> String -> Context
changeOutput context newOutput = { output := newOutput } context
export
compile' : {auto context : Context} -> Bytecode -> Compile String
compile' {context} input = do 
    _ <- runCmd $ "mkdir -p " <+> (context.tempDir)
    _ <- runCmd $ "mkdir -p " <+> (context.buildDir)
    let inputs : List _ = input.modules
    outputAsm : List String <-  traverse (\(out,mod) => assembleLLVM {context=(changeOutput context out)} mod) inputs
    extraAsm : List String <-  traverse (\(out,mod) => assembleForeign {context=(changeOutput context out)} mod) context.extraIr
    linkedBit : String <- linkLLVM {context} (outputAsm ++ extraAsm)
    optimizedBit : String <- optimizeLLVM {context} linkedBit
    compiled <- compileLLVM {context} optimizedBit
    result <- binary {context} compiled
    pure result
export
exec' : {auto context : Context} -> Bytecode -> Compile String
exec' {context} input = do 
    let inputs : List _ = input.modules
    outputAsm : List String <-  traverse (\(out,mod) => assembleLLVM {context=(changeOutput context out)} mod) inputs
    extraAsm : List String <-  traverse (\(out,mod) => assembleForeign {context=(changeOutput context out)} mod) context.extraIr
    linkedBit : String <- linkLLVM {context} (outputAsm ++ extraAsm)
    optimizedBit : String <- optimizeLLVM {context} linkedBit
    result <- runLLVM {context} optimizedBit
    pure result

export
compile : {auto context : Context} -> Bytecode -> IO (Either CompilationError String)
compile = Either.runEitherT . compile'
export 
exec : {auto context : Context} -> Bytecode -> IO (Either CompilationError String)
exec = Either.runEitherT . exec'