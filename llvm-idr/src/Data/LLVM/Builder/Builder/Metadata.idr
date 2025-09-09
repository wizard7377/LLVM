module Data.LLVM.Builder.Builder.Metadata
  
import Data.LLVM.Builder.Types
import Data.LLVM.IR
import Data.LLVM.Builder.Instances
import Data.LLVM.Builder.Util
import Data.LLVM.Builder.State
import Data.LLVM.Builder.Core.Group
export
addMetadata : {m : Type -> Type} -> MonadBuilder m => Annotation -> m ()
addMetadata ann = do
  st <- withinBlock
  case st.statements of 
    Lin => throwError [NoInstruction "Metadata"]
    h :< a => do
      let a' = (the (LStatement -> LStatement) { metadata $= (ann <+>) } $ a)
      let st' = { statements := (h :< a') } st
      modify ({ currentBlock := Just st' })
  pure ()

export 
getDebugInfo : {m : Type -> Type} -> MonadBuilder m => m Annotation
getDebugInfo = pure neutral -- TODO:
export
debugInfo : {m : Type -> Type} -> MonadBuilder m => m ()
debugInfo = getDebugInfo >>= addMetadata
