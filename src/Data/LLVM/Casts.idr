||| Type casting utilities for LLVM IR constructs.
|||
||| This module provides Cast instances that allow automatic conversion
||| between LLVM IR data types. These instances enable seamless integration
||| with Idris's casting system and make the LLVM DSL more ergonomic.
|||
||| The primary purpose is to allow various LLVM constructs to be automatically
||| converted to their appropriate clause types for inclusion in modules,
||| and to provide convenient conversions between related types.
module Data.LLVM.Casts 


import Data.LLVM.Class
import Data.LLVM.Core
import Data.LLVM.Write
import Data.LLVM.Ops
import Data.LLVM.Program

public export
Cast GVarDef LClause where 
    cast d = GlobalDefC d
public export
Cast FunctionDef LClause where 
    cast d = FunctionDefC d
public export
Cast FunctionDec LClause where 
    cast d = FunctionDecC d
public export
Cast Alias LClause where 
    cast d = AliasC d
public export
Cast IFunc LClause where 
    cast d = IFuncC d
--public export
--Cast Metadata LClause where 
--    cast d = MetadataC d
public export
Cast AttributeGroupDef LClause where 
    cast d = AttributeGroupC d
public export
Cast LType FunctionArgSpec where 
    cast t = MkFunctionArgSpec t [] Nothing
public export
Cast Int LConst where 
    cast i =  (LInt i)
public export
Cast String LConst where 
    cast s =  (LString s)
public export
Cast Bool LConst where 
    cast b =  (LBool b)
