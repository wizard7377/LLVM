
module Data.LLVM.Builder.Builder.Function 
  
  
import Data.LLVM.Builder.Types
import Data.LLVM.Builder.State
import Data.LLVM.Builder.Core.Group
import Data.LLVM.Builder.Util
import Data.LLVM.IR

export 
addArg : {m : Type -> Type} -> MonadBuilder m => {default [] attrs : List Attribute} -> {default "" name : String} -> LType -> m ValueRef
addArg {attrs} {name} ty = do 
  name <- defGenName name
  f <- inFunction
  let arg = MkArgument (cast ty) attrs (Just name)
  let f' = { val.args $= (<:: arg) } f
  modify (the ChangeState ({ currentTopLevel := InFunction f' }))
  pure (MkValueRef Nothing ty (localVar name))
