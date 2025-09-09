module Data.LLVM.Builder.Core.Cast 
import Data.LLVM.Builder.Types
import Data.LLVM.Builder.State
import Data.LLVM.Builder.Util
import Data.LLVM.IR
import Data.Table
import Data.SnocList 
import Data.Has
export 
Cast BlockState (String, BasicBlock) where
  cast s = let 
    t = case s.term of 
      Nothing => RetVoid 
      Just t' => t' 
    in (s.name, MkBasicBlock (asList s.statements) t)
export 
Cast BlockState BlockRef where
  cast s = MkBlockRef s.name
   
  
export 
Cast FunctionState FunctionDef where 
  cast f = f.val

export 
Cast TypeRef LType where 
  cast t = t.val

export 
Cast BlockRef Label where 
    cast v = NamedLabel v.label

export 
Cast (ValueRef c) (LValue c) where 
  cast v = v.value
  
export 
Cast (ValueRef True) (LValue False) where 
  cast v = LConst v.value
  
export 
Cast (LValue c) (ValueRef {hasBlock = False} {hasType = False} c) where 
  cast v = MkValueRef Blank Blank v
export
Cast (LValue True) (ValueRef {hasBlock = False} {hasType = False} False) where 
  cast v = MkValueRef Blank Blank (LConst v)
