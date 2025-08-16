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
import Data.LLVM.Write.Assembly

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
Walk LType Argument where 
    go t = MkArgument t [] Nothing
public export
Walk Int LValue where 
    go i =  (LTerm.LInt i)
public export
Walk String LValue where 
    go s =  (LTerm.LString s)
public export
Walk Bool LValue where 
    go b =  (LTerm.LBool b)

public export 
Walk a b => Walk b c => Walk a c where 
  go = go . (the (a -> b) go)
 
public export 
Walk LInstruction LStatement where 
    go op = MkLStatement Nothing op []

public export 
Walk Name LValue where 
    go n = LTerm.LVar n

public export 
Walk Terminator LInstruction where 
  go = TerminatorOp
public export 
Walk (UnaryOpcode, LType, LValue) LInstruction where
  go (a, b, c) = UnaryOp a b c 
 
public export 
Walk (BinaryOpcode, LType, LValue, LValue) LInstruction where
  go (a, b, c, d) = BinaryOp a b c d
 
public export 
Walk VectorOpcode LInstruction where 
  go = VectorOp
public export 
Walk AggregateOpcode LInstruction where 
  go = AggregateOp
  
public export 
Walk (ConversionOpCode, LType, LValue, LType) LInstruction where 
  go (a, b, c, d) = ConversionOp a (withType b c) d
  
public export 
Walk MiscOpcode LInstruction where 
  go = MiscOp
  
public export 
Walk MemoryOpcode LInstruction where 
  go = MemoryOp 
public export 
Walk ExceptOpcode LInstruction where 
  go = ExceptOp
