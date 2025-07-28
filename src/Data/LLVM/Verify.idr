module Data.LLVM.Verify

import Data.LLVM.Core 
import Data.LLVM.Ops 

data HasType : forall k. (v : k) -> (ty : LType) -> Type where 
    Poisened : {ty : LType} -> HasType LPoison ty