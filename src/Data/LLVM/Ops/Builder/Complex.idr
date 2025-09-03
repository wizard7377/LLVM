module Data.LLVM.Ops.Builder.Complex

import Data.LLVM.IR.Core 
import Data.LLVM.IR.Core
import Data.LLVM.IR.Program
import Data.LLVM.Ops.Builder.Types
import Control.Monad.State
genId : Builder Nat 
genId = do
    MkBuilderState currentId typeInfo <- get
    put (MkBuilderState (currentId + 1) typeInfo)
    pure currentId