module Data.LLVM.Builder.Builder.Instructions 

import Data.LLVM.Builder.Types
import Data.LLVM.Builder.State
import Data.LLVM.Builder.Util
import Data.LLVM.Builder.Core.Group
import Data.LLVM.Builder.Core.Cast
import Data.LLVM.IR
import Data.Util
import Data.Has

export 
buildOp : {m : Type -> Type} -> MonadBuilder m => {default Nothing ty : Maybe LType} -> LExpr -> m (ValueRef {hasBlock = True} {hasType = (hasJust ty)} False) 
buildOp {ty} inst = do 
    cb <- withinBlock
    name : String <- genId
    let block' = { statements $= (:< (assign (Local name) inst))} cb
    modify (the (BuilderState -> BuilderState) { currentBlock := Just block' })
    pure (MkValueRef (Given $ cast cb) (maybeHas ty) (localVar name))

export 
buildTerminator : {m : Type -> Type} -> MonadBuilder m => Terminator -> m ()  
buildTerminator term = do 
    cb <- withinBlock
    case cb.term of 
      Just _ => throwError [AlreadyTerminated "Block already has a terminator"]
      Nothing => do 
        let block' = { term := Just term } cb
        modify (the (BuilderState -> BuilderState) { currentBlock := Just block' })
        pure ()
export 
buildFNeg : {m : Type -> Type} -> MonadBuilder m => {default [] fm : FastMath} -> TypeRef -> (ValueRef False) -> m (ValueRef {hasBlock = True} False)
buildFNeg {fm} ty a = buildOp (FNeg fm (cast ty) (cast a))
export
buildAdd : {m : Type -> Type} -> MonadBuilder m => {default NoWrap wrap : Wrapping} -> TypeRef -> (ValueRef False) -> (ValueRef False) -> m (ValueRef False)
buildAdd {wrap} ty a b = buildOp (Add wrap (cast ty) (cast a) (cast b)) 
export 
buildFAdd : {m : Type -> Type} -> MonadBuilder m => {default [] fm : FastMath} -> TypeRef -> (ValueRef False) -> (ValueRef False) -> m (ValueRef False)
buildFAdd {fm} ty a b = buildOp (FAdd fm (cast ty) (cast a) (cast b))

export 
buildRetVoid : {m : Type -> Type} -> MonadBuilder m => m ()
buildRetVoid = buildTerminator RetVoid 
export
buildRet : {m : Type -> Type} -> MonadBuilder m => TypeRef -> (ValueRef False) -> m ()
buildRet ty val = buildTerminator (Ret (cast ty) (cast val))
export 
buildCondBr : {m : Type -> Type} -> MonadBuilder m => (ValueRef False) -> BlockRef -> BlockRef -> m ()
buildCondBr cond t f = buildTerminator (CondBr (cast cond) (cast t) (cast f))
export 
buildJumpBr : {m : Type -> Type} -> MonadBuilder m => BlockRef -> m ()
buildJumpBr target = buildTerminator (JumpBr (cast target))
export 
buildSwitchOp : {m : Type -> Type} -> MonadBuilder m => TypeRef -> (ValueRef False) -> BlockRef -> List CaseBranch -> m ()
buildSwitchOp ty val defaultTarget branches = buildTerminator (Switch (cast ty) (cast val) (cast defaultTarget) branches)
 
export 
buildSwitch : {m : Type -> Type} -> MonadBuilder m => TypeRef -> (ValueRef False) -> BlockRef -> List (ValueRef True, BlockRef) -> m ()
buildSwitch ty val def branches = buildTerminator (Switch (cast ty) (cast val) (cast def) (map go branches))
  where 
    go : (ValueRef True, BlockRef) -> CaseBranch
    go (v, b) = caseBranch (cast ty) (cast v) (cast b)
export 
buildIndirectBr : {m : Type -> Type} -> MonadBuilder m => (ValueRef False) -> List BlockRef -> m ()
buildIndirectBr addr targets = buildTerminator (IndirectBr (cast addr) (map cast targets))

 
export 
buildPhi : {m : Type -> Type} -> MonadBuilder m => {default [] fm : FastMath} -> TypeRef -> List (ValueRef {hb = True} False) -> m (ValueRef False)
buildPhi {fm} ty incoming = buildOp (Phi fm (cast ty) !(traverse go incoming))
  where 
    go : ValueRef False -> m (LValue False, Label)
    go v = do 
        cb <- getBlock
        case cb of 
            Just b => pure (cast v, NamedLabel b.name)
            Nothing => throwError [NoBlock]

buildGEP : {m : Type -> Type} -> MonadBuilder m => {default NoWrap wrap : Wrapping} -> {default GEPAll range : GEPRange} -> TypeRef -> (ValueRef False) -> List (ValueRef False) -> m (ValueRef False)
