||| LLVM IR program structure and top-level definitions.
|||
||| This module defines the data structures for representing complete LLVM IR programs,
||| including global variables, function definitions, and module-level constructs.
||| It provides the building blocks for constructing full LLVM modules.
module Data.LLVM.IR.Program

import Data.LLVM.IR.Core
import Data.LLVM.IR.Core
import Data.Table
||| Global variable definition.
||| Models LLVM IR global variable definitions like:
||| ```llvm
||| @myGlobal = private constant i32 42, align 4
||| @threadVar = thread_local(localdynamic) global i32 0
||| @externInit = external externally_initialized global i32
||| ```
public export
record GVarDef where
  constructor MkGVarDef
  ||| Variable name (without @ prefix)
  name : String 
  ||| Symbol information (linkage, visibility, etc.)
  symbolInfo : SymbolInfo
  ||| Thread-local storage model (thread_local(...))
  threadLocality : Maybe ThreadLocality
  ||| Address significance (unnamed_addr, local_unnamed_addr)
  addressInfo : Maybe AddressInfo
  ||| Address space specification (addrspace(N))
  addressSpace : Maybe AddressSpace
  ||| Whether the variable is externally initialized
  externallyInitialized : Maybe Bool
  ||| Whether this is a constant (true) or global variable (false)
  isConst : Bool 
  ||| The type of the global variable
  gtpe : LType.LType
  ||| Optional initializer value
  initializer : Maybe (LValue True)
  ||| Metadata tags
  tags : Annotation

||| Attribute group definition.
||| Models LLVM IR attribute group definitions like:
||| ```llvm
||| attributes #0 = { nounwind readnone }
||| attributes #1 = { "no-frame-pointer-elim"="true" }
||| ```
public export 
record AttributeGroupDef where
  constructor MkAttributeGroupDef
  ||| Group number (the N in attributes #N)
  name : Nat 
  ||| List of attributes in this group
  attrs : List Attribute


||| Function definition with implementation.
||| Models LLVM IR function definitions like:
||| ```llvm
||| define dso_local i32 @main(i32 %argc, i8** %argv) #0 {
||| entry:
||||   %retval = alloca i32, align 4
||||   ret i32 0
||| }
||| ```
||| Or with more attributes:
||| ```llvm
||| define private fastcc nounwind i32 @helper(i32 %x) 
|||        unnamed_addr section ".text" align 16 gc "shadow-stack" {
||| entry:
||||   ret i32 %x
||| }
||| ```
{-
define [linkage] [PreemptionSpecifier] [visibility] [DLLStorageClass]
       [cconv] [ret attrs]
       <ResultType> @<FunctionName> ([argument list])
       [(unnamed_addr|local_unnamed_addr)] [AddrSpace] [fn Attrs]
       [section "name"] [partition "name"] [comdat [($name)]] [align N]
       [gc] [prefix Constant] [prologue Constant] [personality Constant]
       (!name !N)* { ... }
 -}
public export
record FunctionDef where
  constructor MkFunctionDef
  ||| Function name (without @ prefix)
  name : String 
  ||| Symbol information (linkage, preemption, visibility, DLL storage)
  symbolInfo : SymbolInfo
  ||| Calling convention (fastcc, coldcc, etc.)
  callingConvention : Maybe CallingConvention
  ||| Return value attributes
  returnAttrs : List Attribute
  ||| Return type
  returnType : LType 
  ||| Function parameters with their types and attributes
  args : List Argument
  ||| Address significance (unnamed_addr, local_unnamed_addr)
  addressInfo : Maybe AddressInfo
  ||| Address space (addrspace(N))
  addressSpace : Maybe AddressSpace
  ||| Function attributes (nounwind, readnone, etc.)
  fnAttributes : List Attribute
  ||| Section name (section "name")
  section: Maybe String
  ||| Partition name (partition "name")
  partition: Maybe String
  ||| Comdat group (comdat [($name)])
  comdat: Maybe Name 
  ||| Function alignment (align N)
  alignment : Maybe Int
  ||| Garbage collector name (gc "name")
  gc : Maybe String
  ||| Prefix data (prefix Constant)
  fprefix: Maybe (LValue True)
  ||| Prologue data (prologue Constant)
  prologue: Maybe (LValue True)
  ||| Personality function (personality Constant)
  personality : Maybe (LValue True)
  ||| Attached metadata (!name !N)*
  metadata : List Metadata
  ||| Function body with basic blocks and instructions
  body : Table BasicBlock
  ||| Additional metadata tags
  tags: Annotation
||| Function declaration without implementation.
||| Models LLVM IR function declarations like:
||| ```llvm
||| declare i32 @printf(i8*, ...)
||| declare dso_local void @exit(i32) #1
||| declare external fastcc i32 @external_func(i32, i32)
||| ```
public export
record FunctionDec where
  constructor MkFunctionDec
  ||| Function name (without @ prefix)
  name : String 
  ||| Symbol information (linkage, preemption, visibility, DLL storage)
  symbolInfo : SymbolInfo
  ||| Calling convention (fastcc, coldcc, etc.)
  callingConvention : Maybe CallingConvention
  ||| Return value attributes
  returnAttrs : List Attribute
  ||| Return type
  returnType : LType 
  ||| Function parameters with their types and attributes
  args : List Argument
  ||| Address significance (unnamed_addr, local_unnamed_addr)
  addressInfo : Maybe AddressInfo
  ||| Function alignment (align N)
  alignment : Maybe Int
  ||| Garbage collector name (gc "name")
  gc : Maybe String 
  ||| Prefix data (prefix Constant)
  fprefix: Maybe (LValue True) 
  ||| Prologue data (prologue Constant)
  prologue: Maybe (LValue True)
  ||| Additional metadata tags
  tags: Annotation
||| Alias definition.
||| Models LLVM IR alias definitions like:
||| ```llvm
||| @alias = alias i32, i32* @original
||| @weak_alias = weak alias i8, i8* @target
||| @thread_alias = thread_local alias i32, i32* @tls_var
||| ```
public export  
record Alias where 
  constructor MkAlias
  ||| Alias name (without @ prefix)
  name : String 
  ||| Symbol information (linkage, preemption, visibility, DLL storage)
  symbolInfo : SymbolInfo
  ||| Thread-local storage model
  threadLocality : Maybe ThreadLocality
  ||| Address significance (unnamed_addr, local_unnamed_addr)
  addressInfo : Maybe AddressInfo
  ||| Type of the alias
  aliasTpe : LType
  ||| Name of the aliasee (the target being aliased)
  aliasee : String 
  ||| Additional metadata tags
  tags: Annotation

||| Indirect function (IFunc) definition.
||| Models LLVM IR IFunc definitions like:
||| ```llvm
||| @ifunc = ifunc i32 (i32), i32 (i32)* @resolver
||| @weak_ifunc = weak ifunc void (), void ()* @my_resolver
||| ```
public export
record IFunc where 
  constructor MkIFunc
  ||| IFunc name (without @ prefix)
  name : String 
  ||| Symbol information (linkage, preemption, visibility)
  symbolInfo : SymbolInfo
  ||| Thread-local storage model
  threadLocality : Maybe ThreadLocality
  ||| Address significance (unnamed_addr, local_unnamed_addr)
  addressInfo : Maybe AddressInfo
  ||| Function type
  funTpe : LType
  ||| Resolver function type
  resTpe : LType
  ||| Name of the resolver function
  resolver : String 
  ||| Additional metadata tags
  tags: Annotation

public export 
record TypeDef where 
  constructor MkTypeDef
  name : String 
  ty : LType 
  ann : Annotation
||| Top-level clauses that can appear in an LLVM module.
||| Each clause represents a different kind of top-level declaration.
public export 
data LClause : Type where 
  ||| Global variable definition (@var = ...)
  GlobalDefC : GVarDef -> LClause
  ||| Function definition with body (define ...)
  FunctionDefC : FunctionDef -> LClause
  ||| Function declaration without body (declare ...)
  FunctionDecC : FunctionDec -> LClause
  ||| Alias definition (@alias = alias ...)
  AliasC : Alias -> LClause
  ||| IFunc definition (@ifunc = ifunc ...)
  IFuncC : IFunc -> LClause
  ||| Named metadata (!name = !{...})
  MetadataC : String -> Metadata -> LClause
  ||| Attribute group definition (attributes #N = {...})
  AttributeGroupC : AttributeGroupDef -> LClause
  TypeDefC : TypeDef -> LClause
  ||| Other top-level constructs (e.g., inline assembly, target info)
  OtherC : String -> LClause

||| LLVM Module structure.
||| Models a complete LLVM IR module like:
||| ```llvm
||| target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64..."
||| target triple = "x86_64-pc-linux-gnu"
||| 
||| @global_var = global i32 42
||| 
||| define i32 @main() {
||||   ret i32 0
||| }
||| ```
public export
record LModule where 
  constructor MkLModule
  ||| Target data layout string (target datalayout = "...")
  dataLayout : Maybe String
  ||| Target triple string (target triple = "...")
  target : Maybe String
  ||| List of top-level declarations and definitions
  text: List LClause
  ||| Module-level metadata tags
  tags: Annotation
-- TODO: Comdats
-- TODO: fin param attributes

||| LLVM Bytecode container.
||| Represents a collection of LLVM modules, typically used for
||| multi-module compilation units or linked programs.
public export 
record Bytecode where
  constructor MkBytecode
  mainMod : Maybe String
  ||| List of LLVM modules in this bytecode unit
  modules : Table LModule
