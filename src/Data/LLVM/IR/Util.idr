module Data.LLVM.IR.Util

import Data.LLVM.IR.Core
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Program
import Data.LLVM.Class

{- 
public export 
interface LBasicBlock a where 
    constructor MkBasicBlock
    resolve : Destination -> a -> List LStatement 

export
implementation LBasicBlock (Destination -> List LStatement) where 
    resolve n f = f n

export
implementation LBasicBlock (List LStatement, LInstruction) where 

    resolve ( n) (stmt, op) = stmt ++ [MkLStatement n op]

-}