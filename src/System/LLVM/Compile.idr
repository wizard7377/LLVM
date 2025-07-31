module System.LLVM.Compile

import System.LLVM.Common
import Data.LLVM
import System
export
compileLLVM : {auto context : Context} -> String -> Compile String
compileLLVM {context} input = do
    let cmd = "llc -filetype=obj -o " ++ (context.buildDir <+> context.output <+> ".o") ++ " " ++ (context.buildDir <+> "OPT_" <+> input <+> ".bc")
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ CompileError out)
    pure input 
    



