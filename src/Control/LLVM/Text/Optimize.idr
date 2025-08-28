module Control.LLVM.Text.Optimize

import Control.LLVM.Text.Common
import Data.LLVM.IR.Program
import System
import Control.LLVM.Stage
export
optimizeLLVM : String -> Stage String
optimizeLLVM input = do
    let cmd = "opt -o " ++ (context.buildDir <+> "OPT_" <+> context.output <+> ".bc") ++ " " ++ (context.buildDir <+> input <+> ".bc")
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ OptimizeError out)
    pure input 


