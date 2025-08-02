||| Type casting utilities for LLVM IR constructs.
|||
||| This module provides Walk instances that allow automatic conversion
||| between LLVM IR data types. These instances enable seamless integration
||| with Idris's casting system and make the LLVM DSL more ergonomic.
|||
||| The primary purpose is to allow various LLVM constructs to be automatically
||| converted to their appropriate clause types for inclusion in modules,
||| and to provide convenient conversions between related types.
module Data.LLVM.Casts 


import Data.LLVM.Class
import Data.LLVM.IR
import Data.LLVM.Write
import Data.Walk
public export
Walk GVarDef LClause where 
    go d = GlobalDefC d
public export
Walk FunctionDef LClause where 
    go d = FunctionDefC d
public export
Walk FunctionDec LClause where 
    go d = FunctionDecC d
public export
Walk Alias LClause where 
    go d = AliasC d
public export
Walk IFunc LClause where 
    go d = IFuncC d
--public export
--Walk Metadata LClause where 
--    go d = MetadataC d
public export
Walk AttributeGroupDef LClause where 
    go d = AttributeGroupC d
public export
Walk LType FunctionArgSpec where 
    go t = MkFunctionArgSpec t [] Nothing
public export
Walk Int LExpr where 
    go i =  (LInt i)
public export
Walk String LExpr where 
    go s =  (LString s)
public export
Walk Bool LExpr where 
    go b =  (LBool b)

public export 
Walk a b => Walk b c => Walk a c where 
  go = go . (the (a -> b) go)
 
public export 
Walk LOperation LStatement where 
    go op = Operation Trash op

public export 
Walk Name LExpr where 
    go n = LVar n

public export 
Walk Terminator LOperation where 
  go = TerminatorOp
public export 
Walk (UnaryOpcode, LType, LExpr) LOperation where
  go (a, b, c) = UnaryOp a b c 
 
public export 
Walk (BinaryOpcode, LType, LExpr, LExpr) LOperation where
  go (a, b, c, d) = BinaryOp a b c d
 
public export 
Walk VectorOpcode LOperation where 
  go = VectorOp
public export 
Walk AggregateOpcode LOperation where 
  go = AggregateOp
  
public export 
Walk (ConversionOpCode, LType, LExpr, LType) LOperation where 
  go (a, b, c, d) = ConversionOp a (withType b c) d
  
public export 
Walk MiscOpcode LOperation where 
  go = MiscOp
  
public export 
Walk MemoryOpcode LOperation where 
  go = MemoryOp 
public export 
Walk ExceptOpcode LOperation where 
  go = ExceptOp

public export 
Walk (List LStatement) Block where 
  go = MkBlock
