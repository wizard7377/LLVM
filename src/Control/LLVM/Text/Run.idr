module Control.LLVM.Text.Run

import Control.LLVM.Text.Common
import Data.LLVM.IR.Program
import System
import Control.LLVM.Stage
export
runLLVM : String -> Stage String
runLLVM input = do
    let cmd = "lli " ++ (context.tempDir <+> input <+> ".bc")
    r <- runCmd cmd
    (out, r) <- runCmd cmd
    (unless $ r > 0) (throwError $ RunError out)
    pure input

