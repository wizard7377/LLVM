module Data.LLVM.Logic.Infer

import Control.Monad.RWS
import Data.LLVM.IR


interface Infer (a : Type) where 
    context : Type 
    canInfer : context -> a -> Type 
    infer : {c : context} -> (v : a) -> {auto p : canInfer c v} -> LType
