module Control.LLVM.Text.Assemble 
import Control.LLVM.Stage
export
||| Compile LLVM IR in a bitcode
assemble : Code -> Stage Code 
assemble c = do 
    showMsg "Assembling"
    f <- asFile c 
    o <- newFile 
    showMsg "Assembling 2"
    (sout, res) <- runCmd $ "llvm-as " ++ " -o " ++ o ++ " " ++ f 
    showMsg "Assembling 3"
    (unless $ res == 0) (throwError $ [CompileError sout])
    showMsg "Assembling 4"
    pure $ fileToCode o
