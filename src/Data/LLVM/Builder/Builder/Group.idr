module Data.LLVM.Builder.Builder.Group

import Data.LLVM.Builder.Types
import Data.LLVM.Builder.State
import Data.LLVM.Builder.Builder.Core
import Data.LLVM.Builder.Util
import Data.SortedMap
import Data.LLVM.IR.Core
export 
function : MonadBuilder m => {default "" name : String} -> m FunctionRef 

export 
block : MonadBuilder m => {default "" name : String} -> m BlockRef

export 
declare : MonadBuilder m => {default "" name : String} -> m FunctionRef 
