module Data.LLVM.Builder.Core.Core

import Data.LLVM.Builder.Types
import Data.LLVM.IR
import Data.LLVM.IR.Util
import Data.LLVM.Builder.Instances
import Data.LLVM.Builder.Util
import Control.Monad.State
import Control.Monad.Either
import Data.LLVM.Builder.State

export 
getTopLevel : {m : Type -> Type} -> MonadBuilder m => m TopLevel
getTopLevel = do  
  st <- get  
  pure st.currentTopLevel

export 
modifyTopLevel : {m : Type -> Type} -> MonadBuilder m => (TopLevel -> TopLevel) -> m ()
modifyTopLevel f = modify { currentTopLevel $= f }

export 
setTopLevel : {m : Type -> Type} -> MonadBuilder m => TopLevel -> m ()
setTopLevel tl = modify { currentTopLevel := tl } 

export 
popTopLevel : {m : Type -> Type} -> MonadBuilder m => m TopLevel
popTopLevel = do 
  tl <- getTopLevel 
  setTopLevel InNothing
  pure tl
export
logMsg : {default 0 verb : Int} -> {default Info ty : MsgType} -> String -> {m : Type -> Type} -> MonadBuilder m => m ()
logMsg {verb} {ty} msg = do 
  tell (MkBuilderLog [(ty, msg, verb)])
  
export 
appendClause : {m : Type -> Type} -> MonadBuilder m => LClause -> m ()
appendClause c = modify { clauses $= (c ::) }
