module System.LLVM.Assemble

import System.LLVM.Common
import Data.LLVM.Program
import Data.LLVM.Class
import Data.LLVM.Write
import System
import Control.App
import System.File.ReadWrite
export 
assembleLLVM : {context : Context} -> LModule -> String -> Compile String
assembleLLVM {context} mod output = do 
    liftIO $ putStrLn $ "Assembling LLVM module: " ++ output
    liftIO $ putStrLn $ "From module" ++ output
    let encoded' : VString = encode mod
    let encoded = show encoded'
    let fileName = context.tempDir <+> output <+> ".ll"
    _ <- liftIO $ writeFile fileName encoded
    let cmd = "llvm-as -o " <+> (context.tempDir <+> output <+> ".bc") <+> " " <+> fileName 
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ AssembleError out)
    pure output





