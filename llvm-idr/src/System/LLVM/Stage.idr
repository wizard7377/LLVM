module System.LLVM.Stage 

import System.LLVM.Common

public export 
Stage : Type -> Type -> Type 
Stage input output = {auto context : Context} -> input -> Compile output

export 
runStage : {input, output : Type} -> Stage input output -> {auto context : Context} -> input -> Compile output
runStage {context} stage input = stage {context} input