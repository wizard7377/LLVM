module System.LLVM.Run

import System.LLVM.Common
import Data.LLVM.Program
import System

export
runLLVM : {auto context : Context} -> String -> Compile String
runLLVM {context} input = do
    let cmd = "lli " ++ (context.tempDir <+> input <+> ".bc")
    r <- runCmd cmd
    (out, r) <- runCmd cmd
    (unless $ r > 0) (throwError $ RunError out)
    pure input


