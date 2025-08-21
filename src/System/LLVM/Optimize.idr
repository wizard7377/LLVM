module System.LLVM.Optimize

import System.LLVM.Common
import Data.LLVM.IR.Program
import System
import System.LLVM.Stage
export
optimizeLLVM : {auto context : Context} -> String -> Compile String
optimizeLLVM {context} input = do
    let cmd = "opt -o " ++ (context.buildDir <+> "OPT_" <+> context.output <+> ".bc") ++ " " ++ (context.buildDir <+> input <+> ".bc")
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ OptimizeError out)
    pure input 

export 
[optimizeLLVMStage] Stage String String where
    runStage {context} input = optimizeLLVM {context} input