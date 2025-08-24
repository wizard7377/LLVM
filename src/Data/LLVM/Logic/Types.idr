module Data.LLVM.Logic.Types
import Data.LLVM.IR
 
public export 
record Context where 
  constructor MkContext
  typing : List (Name, Type)
  values : List (Name, (LValue True))

public export 
emptyContext : Context 
emptyContext = MkContext [] []

public export 
interface InferType (a : Type) where 
  canInfer : {default emptyContext ctx : Context} -> (v : a) -> Type 
  inferType : {default emptyContext ctx : Context} -> (v : a) -> {auto p : canInfer {ctx=ctx} v} -> LType

