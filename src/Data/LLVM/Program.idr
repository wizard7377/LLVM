module Data.LLVM.Program

import Data.LLVM.Core
import Data.LLVM.Ops


public export
record GVarDef where
  constructor MkGVarDef
  name : String 
  symbolInfo : SymbolInfo
  threadLocality : Maybe ThreadLocality
  addressInfo : Maybe AddressInfo
  addressSpace : Maybe AddressSpace
  externallyInitialized : Maybe Bool
  isConst : Bool 
  gtpe : LType.LType
  initializer : Maybe String
  tags : List LTag


public export 
record FunctionBody where 
  constructor MkFunctionBody
  
public export
record FunctionDef where
  constructor MkFunctionDef
  name : String 
  symbolInfo : SymbolInfo
  callingConvention : Maybe CallingConvention
  returnAttrs : List Attribute
  returnType : LType 
  args : List FunctionArgSpec
  addressInfo : Maybe AddressInfo
  addressSpace : Maybe AddressSpace
  tags: List LTag
public export
record FunctionDec where
  constructor MkFunctionDec
  name : String 
  symbolInfo : SymbolInfo
  callingConvention : Maybe CallingConvention
  returnAttrs : List Attribute
  returnType : LType 
  args : List FunctionArgSpec
  addressInfo : Maybe AddressInfo
  alignment : Maybe Int
  gc : Maybe String 
  fprefix: Maybe LConst 
  prologue: Maybe LConst
  tags: List LTag
public export  
record Alias where 
  constructor MkAlias
  name : String 
  symbolInfo : SymbolInfo
  threadLocality : Maybe ThreadLocality
  addressInfo : Maybe AddressInfo
  aliasTpe : LType
  ptrType : LType
  aliasee : String 
  tags: List LTag
public export
record IFunc where 
  constructor MkIFunc
  name : String 
  symbolInfo : SymbolInfo
  threadLocality : Maybe ThreadLocality
  addressInfo : Maybe AddressInfo
  funTpe : LType
  resTpe : LType
  resolver : String 
  tags: List LTag

public export
record LModule where 
  constructor MkLModule
  globals : List GVarDef
  functions : List FunctionDef
  aliases : List Alias
  ifuncs : List IFunc 
  declarations : List FunctionDec
  metadata : List Metadata
  dataLayout : Maybe String
-- TODO: Comdats
-- TODO: fin param attributes
-- TODO: Bundles

