module Data.LLVM.Builder.Core.Group

import Data.LLVM.Builder.Types
import Data.LLVM.Builder.State
import Data.LLVM.Builder.Core.Core
import Data.LLVM.Builder.Util
import Data.SortedMap
import Data.LLVM.IR
import Data.LLVM.Ops
import Data.Table
import Data.LLVM.Builder.Core.Cast
import Data.LLVM.Builder.Core.Ops 
  
export 
inFunction : {m : Type -> Type} -> MonadBuilder m => m FunctionState
inFunction = do 
  st <- get 
  case st.currentTopLevel of 
    InFunction f => the (m ?) $ pure f 
    _ => throwError [OutsideFunction "Not currently in a function"]
  
export 
withinBlock : {m : Type -> Type} -> MonadBuilder m => m BlockState
withinBlock = do 
  st <- get 
  case st.currentBlock of 
    Just b => pure b 
    Nothing => throwError [NoBlock]
export 
endFunction : {m : Type -> Type} -> MonadBuilder m => m ()
endFunction = do 
  st : BuilderState <- get
  case st.currentTopLevel of 
    InFunction f => do 
        modify ({ currentTopLevel := InNothing })
        modify ({ clauses $= (:< (FunctionDefC $ cast f)) })
        logMsg $ "Ended function: " ++ (show f.name)
        pure ()
    _ => throwError [OutsideFunction "Not currently in a function"]

    
export
endDeclare : MonadBuilder m => m ()

export
isDeclare : MonadBuilder m => m Bool
isDeclare = do 
  st <- get 
  case st.currentTopLevel of 
    InDeclare _ => pure True 
    _ => pure False
export
isBlock : MonadBuilder m => m Bool
isBlock = do 
  st <- get 
  case st.currentBlock of 
    Just _ => pure True 
    _ => pure False
  
export
isFunction : MonadBuilder m => m Bool  
isFunction = do 
  st <- get 
  case st.currentTopLevel of 
    InFunction _ => pure True 
    _ => pure False
  
isTopLevel : MonadBuilder m => m Bool
isTopLevel = do 
  st <- get 
  case st.currentTopLevel of 
    InNothing => pure True 
    _ => pure False
  
export 
emptyTopLevel : {m : Type -> Type} -> MonadBuilder m => m () 
emptyTopLevel = do 
  st <- get 
  case st.currentTopLevel of 
    InNothing => logMsg "Nothing to empty"
    InFunction f => logMsg "Ending function" >> endFunction
    InDeclare d => logMsg "Ending declare" >> endDeclare
export 
function : {m : Type -> Type} -> MonadBuilder m => {default "" name : String} -> {default LVoid ty : LType} -> m FunctionRef 
function {name} {ty} = do 
  st <- get
  name' <- if name == "" then internalSpace <$> genId else pure $ userSpace name
  emptyTopLevel 
  let currentFunction = MkFunctionState name' Lin (functionDef' name' ty [] []) 
  setTopLevel (InFunction currentFunction)
  pure $ MkFunctionRef name'

export 
newBlock : {m : Type -> Type} -> MonadBuilder m => {default "" name : String} -> m BlockState
newBlock {name} = do 
  name' <- if name == "" then internalSpace <$> genId else pure $ userSpace name
  pure $ mkBlockState Lin 
  
export 
setBlock : {m : Type -> Type} -> MonadBuilder m => BlockState -> m ()
setBlock b = do 
  st <- get 
  case st.currentTopLevel of 
    InFunction _ => do 
      modify ({ currentBlock := Just b })
      logMsg $ "Set current block to: " ++ b.name
      pure ()
    _ => throwError [OutsideFunction "Not currently in a function"]
 
export 
getBlock : {m : Type -> Type} -> MonadBuilder m => m (Maybe BlockState)
getBlock = do 
  st <- get 
  pure st.currentBlock
export 
popBlock : {m : Type -> Type} -> MonadBuilder m => m (Maybe BlockState)
popBlock = do 
  cb <- getBlock
  modify ({ currentBlock := Nothing })
  pure cb 
export
switchBlock : {m : Type -> Type} -> MonadBuilder m => BlockState -> m BlockState 
switchBlock newBlock = do 
  oldBlock <- getBlock
  setBlock newBlock
  case oldBlock of 
    Just b => pure b 
    Nothing => throwError [NoBlock]
export
insertBegin : {m : Type -> Type} -> MonadBuilder m => BlockState -> m BlockRef
insertBegin block = do 
  st <- get 
  case st.currentTopLevel of 
    InFunction f => do 
      let f' = (uncurry appendBegin) (cast block) f
      logMsg $ "Inserted at beginning of block: " ++ block.name
      modify ({ currentTopLevel := InFunction f' })
    _ => throwError [OutsideFunction "Not currently in a function"]
  pure $ label block
export
insertEnd : {m : Type -> Type} -> MonadBuilder m => BlockState -> m BlockRef
insertEnd block = do 
  st <- get 
  case st.currentTopLevel of 
    InFunction f => do 
      let f' = (uncurry appendEnd) (cast block) f
      logMsg $ "Inserted at beginning of block: " ++ block.name
      modify ({ currentTopLevel := InFunction f' })
    _ => throwError [OutsideFunction "Not currently in a function"]
  pure (label block)
export 
declare : MonadBuilder m => {default "" name : String} -> m FunctionRef 

  
export
buildBegin : {m : Type -> Type} -> MonadBuilder m => m () 
buildBegin = do 
  cb <- popBlock 
  _ <- traverse insertBegin cb
  pure ()
export
buildEnd : {m : Type -> Type} -> MonadBuilder m => m ()
buildEnd = do 
  cb <- popBlock 
  _ <- traverse insertEnd cb
  pure ()
 
export
||| Scope changes within to a given block, restoring the previous block on exit
scopeBlock : {m : Type -> Type} -> MonadBuilder m => m a -> m (BlockRef, a)
scopeBlock action = do 
  cb <- getBlock
  nb <- newBlock
  setBlock nb 
  result <- action
  buildEnd
  case cb of 
    Just b => setBlock b 
    Nothing => modify ({ currentBlock := Nothing })
  pure (cast nb, result)
export
scopeBlock_ : {m : Type -> Type} -> MonadBuilder m => m a -> m BlockRef
scopeBlock_ action = do 
  (r , result) <- scopeBlock action
  pure r
 

