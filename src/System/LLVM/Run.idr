module System.LLVM.Run

import System.LLVM.Common
import Data.LLVM.IR.Program
import System
import System.LLVM.Stage
export
runLLVM : {auto context : Context} -> String -> Compile String
runLLVM {context} input = do
    let cmd = "lli " ++ (context.tempDir <+> input <+> ".bc")
    r <- runCmd cmd
    (out, r) <- runCmd cmd
    (unless $ r > 0) (throwError $ RunError out)
    pure input

export
[runLLVMStage] Stage String String where
    runStage {context} input = runLLVM {context} input

