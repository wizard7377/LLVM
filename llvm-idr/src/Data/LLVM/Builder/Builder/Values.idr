module Data.LLVM.Builder.Builder.Values

import Data.LLVM.Builder.Types
import Data.LLVM.Builder.State
import Data.LLVM.Builder.Util
import Data.LLVM.IR 
buildInt : {m : Type -> Type} -> MonadBuilder m => {default Nothing ty : Maybe LType} -> Int -> m ValueRef
buildInt {ty} val = do
  let lit = mkInt val
  pure $ MkValueRef Nothing ty lit
    
