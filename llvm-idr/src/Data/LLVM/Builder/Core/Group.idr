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
maybeGenName : {m : Type -> Type} -> MonadBuilder m => Show a => Maybe a -> m String
maybeGenName Nothing = internalSpace <$> genId
maybeGenName (Just n) = userSpace <$> pure (show n)
export
endBlock : {m : Type -> Type} -> MonadBuilder m => m ()
endBlock = do 
    st : BuilderState <- get
    block <- 
      case st.currentBlock of 
        Just b => pure b 
        Nothing => throwError [NoBlock]
    let (name1, block1) : (String, BasicBlock) = cast block  
    f <- inFunction
    let f1 = appendBlock name1 block1 f
    modify ({ currentBlock := Nothing, currentTopLevel := InFunction f1 })
    logMsg $ "Ended block: " ++ name1
    pure ()
export 
endFunction : {m : Type -> Type} -> MonadBuilder m => m ()
endFunction = do 
  st : BuilderState <- get
  case st.currentTopLevel of 
    InFunction f => do 
        perhaps endBlock
        modify ({ currentTopLevel := InNothing })
        modify ({ clauses $= ((FunctionDefC $ cast f) ::) })
        logMsg $ "Ended function: " ++ (show f.name)
        pure ()
    _ => throwError [OutsideFunction "Not currently in a function"]

    
export
endDeclare : MonadBuilder m => m ()

isDeclare : MonadBuilder m => m Bool
isDeclare = do 
  st <- get 
  case st.currentTopLevel of 
    InDeclare _ => pure True 
    _ => pure False
isBlock : MonadBuilder m => m Bool
isBlock = do 
  st <- get 
  case st.currentBlock of 
    Just _ => pure True 
    _ => pure False
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
  let currentFunction = MkFunctionState name' (functionDef' name' ty [] [])
  setTopLevel (InFunction currentFunction)
  pure $ MkFunctionRef name'
export
block : {m : Type -> Type} -> MonadBuilder m => {default "" name : String} -> m BlockRef    
block {name} = do 
  st <- get 
  unless !isFunction $ throwError [OutsideFunction $ "Cannot start a block outside of a function"]
  perhaps endBlock
  name' <- (if (name == "") then (internalSpace <$> genId) else (pure $ userSpace name))
  modify ({ currentBlock := Just (MkBlockState [] Nothing empty name') })
  pure $ MkBlockRef name'
  
export 
declare : MonadBuilder m => {default "" name : String} -> m FunctionRef 
