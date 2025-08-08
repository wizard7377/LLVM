module Data.LLVM.IR.Util

import Data.LLVM.IR.Core
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Program
import Data.LLVM.Class
public export 
interface LBlock a where 
    constructor MkBlock
    resolve : Destination -> a -> List LStatement 

export
implementation LBlock (Destination -> List LStatement) where 
    resolve n f = f n

export
implementation LBlock (List LStatement, LOperation) where 

    resolve ( n) (stmt, op) = stmt ++ [Operation n op]