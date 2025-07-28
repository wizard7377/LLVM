module Data.LLVM.Logic.Types
import Data.LLVM.Core
record Context where 
  constructor MkContext
  typing : List (Name, Type)
  values : List (Name, LExpr)
