module Data.LLVM.IR.Util

import Data.LLVM.IR.Core
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Program
import Data.LLVM.Class
public export 
interface LBlock a where 
    constructor MkBlock
    resolve : Maybe Name -> a -> List LStatement 

export
implementation LBlock (Maybe Name -> List LStatement) where 
    resolve n f = f n

export
implementation LBlock (List LStatement, LOperation) where 
    resolve (Nothing) (stmt, op) = stmt ++ [Operation Trash op]
    resolve (Just n) (stmt, op) = stmt ++ [Operation n op]