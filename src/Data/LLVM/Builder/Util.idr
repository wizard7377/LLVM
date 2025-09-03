module Data.LLVM.Builder.Util 

import Data.LLVM.Builder.Types
import Data.LLVM.IR
import Data.LLVM.IR.Util
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Either
import Control.Monad.RWS
import Data.LLVM.Builder.Instances

export 
getId : {m : Type -> Type} -> MonadBuilder m => m Int
getId = do
    st : BuilderState <- get 
    modify { uid $= (+ 1) }
    pure st.uid 
export 
genId : {m : Type -> Type} -> MonadBuilder m => m String 
genId = do 
    st : BuilderState <- get 
    let newId = "idris.LLVM.Internal.Builder.Gen" ++ show (st.uid) 
    modify { uid $= (+ 1) }
    pure newId

 
