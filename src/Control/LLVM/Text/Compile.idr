module Control.LLVM.Text.Compile

import Control.LLVM.Text.Common
import Data.LLVM
import System
import Control.LLVM.Stage
export
compileLLVM : String -> Stage String
compileLLVM input = do
    let cmd = "llc -filetype=obj -o " ++ (context.buildDir <+> context.output <+> ".o") ++ " " ++ (context.buildDir <+> "OPT_" <+> input <+> ".bc")
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ CompileError out)
    pure input 
    


