module Control.LLVM.Text.Compile
import Control.LLVM.Stage
import Control.LLVM.Code
  
export
||| Convert from bitcode to machine assembly
compileObj : Code -> Stage Code 
compileObj c = do 
    context <- ask 
    f <- asFile c 
    o <- newFile
    let cmd = "llc --filetype obj -o " ++ o ++ " " ++ f
    (sout, res) <- runCmd cmd
    (unless $ res == 0) (throwError $ [CompileError sout])
    pure $ fileToCode o
export
compileAsm : Code -> Stage Code 
compileAsm c = ?todo
