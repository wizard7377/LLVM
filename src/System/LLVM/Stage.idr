module System.LLVM.Stage 

import System.LLVM.Common

public export 
interface Stage input output where 
    runStage : {auto context : Context} -> input -> Compile output