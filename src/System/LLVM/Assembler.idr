module System.LLVM.Assembler

import System.LLVM.Common
import Data.LLVM
import System
import Control.App
import System.File.ReadWrite
import Control.Monad.Identity
export 
assembleLLVM : {context : Context} -> LModule -> String -> Compile String
assembleLLVM {context} mod output = do 
    showMsg $ "Assembling LLVM module: " ++ output
    showMsg $ "From module" ++ output
    let encoded' : VString = runIdentity (encode mod)
    let encoded = show encoded'
    let fileName = context.tempDir <+> output <+> ".ll"
    _ <- liftIO $ writeFile fileName encoded
    let cmd = "llvm-as -o " <+> (context.tempDir <+> output <+> ".bc") <+> " " <+> fileName 
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ AssembleError out)
    pure output



export 
assembleForeign : {context : Context} -> String -> String -> Compile String
assembleForeign {context} mod output = do 
    let cmd = "llvm-as -o " <+> (context.tempDir <+> output <+> ".bc") <+> " " <+> mod 
    (out, r) <- runCmd cmd
    (unless $ r == 0) (throwError $ AssembleError out)
    pure output