module Data.LLVM.Builder.Core.Cast 
import Data.LLVM.Builder.Types
import Data.LLVM.Builder.State
import Data.LLVM.Builder.Util
import Data.LLVM.IR
import Data.Table
  
export 
Cast BlockState (String, BasicBlock) where
  cast s = let 
    t = case s.term of 
      Nothing => RetVoid 
      Just t' => t' 
    in (s.name, MkBasicBlock s.statements t)
  
  
export 
Cast FunctionState FunctionDef where 
  cast f = f.val
