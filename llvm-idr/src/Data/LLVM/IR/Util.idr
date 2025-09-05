module Data.LLVM.IR.Util

import Data.LLVM.IR.Core
import Data.LLVM.IR.Core
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
implementation LBasicBlock (List LStatement, LExpr) where 

    resolve ( n) (stmt, op) = stmt ++ [MkLStatement n op]

-}
public export 
interface CanNote a where 
    note : a -> Annotation -> a
  

public export 
implementation CanNote LStatement where 
  note e a = { metadata $= (<+> a) } e

  
||| A very simple lens that sets a value
public export 
interface Setter a b where 
  setting : (b -> b) -> (a -> a)
