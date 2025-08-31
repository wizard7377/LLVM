module Control.LLVM.Text.Combine 

import Control.LLVM.Stage
import Control.LLVM.Code
export
||| Combine multiple LLVM modules into one.
combine : List Code -> Stage Code
combine cs = do 
    showMsg "Combining"
    fs <- traverseStage asFile cs
    out <- newFile
    let cmd = "llvm-link -o " ++ out ++ intercalate " " fs 
    (sout, r) <- runCmd cmd
    (unless $ r == 0) (throwError [OtherError "combien error"])
    let out' = fileToCode out
    pure out'
