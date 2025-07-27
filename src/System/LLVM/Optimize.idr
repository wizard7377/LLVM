module System.LLVM.Optimize

import System.LLVM.Common
import Data.LLVM.Program
import System
export
optimizeLLVM : {auto context : Context} -> String -> Compile String
optimizeLLVM {context} input = do
    let cmd = "opt -o " ++ (context.buildDir <+> "OPT_" <+> context.output <+> ".bc") ++ " " ++ (context.buildDir <+> input <+> ".bc")
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ OptimizeError out)
    pure input 