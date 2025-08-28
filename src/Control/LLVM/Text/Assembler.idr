module Control.LLVM.Text.Assembler

import Control.LLVM.Text.Common
import Data.LLVM
import System
--import Control.App
import System.File.ReadWrite
import Control.Monad.Identity
import Control.LLVM.Stage
import Control.Monad.Reader
export 
assembleLLVM : LModule -> Stage String
assembleLLVM {context} mod = do 
    let output = context.output
    showMsg $ "Assembling LLVM module: " ++ output
    showMsg $ "From module" ++ output
    let encoded' : VString = runATM (encode mod)
    let encoded = show encoded'
    let fileName = context.tempDir <+> output <+> ".ll"
    _ <- liftIO $ writeFile fileName encoded
    let cmd = "llvm-as -o " <+> (context.tempDir <+> output <+> ".bc") <+> " " <+> fileName 
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ AssembleError out)
    pure output



export 
assembleForeign : String -> Stage String
assembleForeign {context} mod = do 
    let output = context.output
    let cmd = "llvm-as -o " <+> (context.tempDir <+> output <+> ".bc") <+> " " <+> mod 
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ AssembleError out)
    pure output

