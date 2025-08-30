module Control.LLVM.Text.Assemble 
import Control.LLVM.Stage
export
||| Compile LLVM IR in a bitcode
assemble : Code -> Stage Code 
assemble c = do 
    f <- asFile c 
    o <- newFile 
    (sout, res) <- runCmd $ "llvm-as " ++ " -o " ++ o ++ f 
    (unless $ res == 0) (throwError $ [CompileError sout])
    pure $ fileToCode o
