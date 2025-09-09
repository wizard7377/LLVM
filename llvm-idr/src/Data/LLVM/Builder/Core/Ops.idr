module Data.LLVM.Builder.Core.Ops

import Data.LLVM.Builder.Types
import Data.LLVM.Builder.State
import Data.LLVM.Builder.Util
import Data.LLVM.IR
import Data.Table
import Data.Has
export 
appendBegin : String -> BasicBlock -> FunctionState -> FunctionState 
appendBegin name b f = ({ body $= ((pure (name, b)) <+> )} f)
export 
appendEnd : String -> BasicBlock -> FunctionState -> FunctionState 
appendEnd name b f = ({ body $= (:< (name, b))} f)

public export 
inside : {0 p, q : Bool} -> ValueRef {hasBlock=p} {hasType=q} c -> BlockRef -> ValueRef {hasBlock=True} {hasType=q} c
inside v b = { inBlock := Given b } v
  
public export 
withType : ValueRef {hasBlock=p} {hasType=q} c -> LType -> ValueRef {hasBlock=p} {hasType = True} c
withType v t = { ty := Given t } v
