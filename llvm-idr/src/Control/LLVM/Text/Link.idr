module Control.LLVM.Text.Link 
import Control.LLVM.Stage
import Control.LLVM.Code


export
link : Code -> Stage Code
link c = do
    showMsg "Linking"
    context <- ask
    f <- asFile c 
    o <- newFile 
    let cmd = "ld -o " ++ o ++ " " ++ f
    (sout, r) <- runCmd cmd
    (unless $ r == 0) (throwError [OtherError "link error"])
    let out' = fileToCode o
    pure out'
