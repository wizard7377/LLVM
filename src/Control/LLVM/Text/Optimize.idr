module Control.LLVM.Text.Optimize 

import Control.LLVM.Stage
import Control.LLVM.Code

export
optimize : Code -> Stage Code
optimize c = do 
    context <- ask 
    pure c -- TODO:
