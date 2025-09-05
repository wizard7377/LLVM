module Data.LLVM.Builder.Core.Ops

import Data.LLVM.Builder.Types
import Data.LLVM.Builder.State
import Data.LLVM.Builder.Util
import Data.LLVM.IR
import Data.Table
export 
appendBlock : String -> BasicBlock -> FunctionState -> FunctionState 
appendBlock name b f = ({ val.body $= ((name, b) ::) } f)
