module Data.LLVM.Pass

public export 
data Pass : Type where 
    Level : Int -> Pass 