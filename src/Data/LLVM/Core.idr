module Data.LLVM.Core
  

public export
data Linkage : Type where
  Private      : Linkage
  Internal     : Linkage
  Available    : Linkage
  LinkOnce     : Linkage
  Weak         : Linkage
  Common       : Linkage
  Appending    : Linkage
  ExternWeak   : Linkage
  LinkOnceODR  : Linkage
  WeakODR      : Linkage
  External     : Linkage

public export
data CallingConvention : Type where
  C              : CallingConvention
  Fast           : CallingConvention
  Cold           : CallingConvention
  GHC            : CallingConvention
  CC11           : CallingConvention
  AnyReg         : CallingConvention
  PreserveMost   : CallingConvention
  PreserveAll    : CallingConvention
  PreserveNone   : CallingConvention
  CxxFastTL      : CallingConvention
  Tail           : CallingConvention
  Swift          : CallingConvention
  SwiftTail      : CallingConvention
  CFGuardCheck   : CallingConvention
  CustomCC       : Int -> CallingConvention

public export
data Visibility : Type where
  Default   : Visibility
  Hidden    : Visibility
  Protected : Visibility

public export
data DLLStorage : Type where
  DLLExport : DLLStorage
  DLLImport : DLLStorage

public export
data ThreadLocality : Type where
  LocalDynamic : ThreadLocality
  InitialExec  : ThreadLocality
  LocalExec    : ThreadLocality

public export
data Preemption : Type where
  Preemptible    : Preemption
  NonPreemptible : Preemption

public export
data AddressInfo : Type where
  UnnamedGlobal : AddressInfo
  UnnamedLocal  : AddressInfo

-- data layout
public export
data AddressSpace = NamedSpace String | UnnamedSpace Int
public export
data Name : Type where 
  ||| %...
  Local : String -> Name
  ||| \@...
  Global : String -> Name
  ||| \$...
  Special : String -> Name
  ||| !...
  MetadataN : String -> Name
  ||| #...
  AttributeN : String -> Name
  ||| ...: 
  LabelN : String -> Name
  ||| "@llvm."...
  IntrinsicN : String -> Name
  ||| User defined
  CustomN : String -> Name

namespace LType 
  public export 
  data LTypeF : Type where
    Half      : LTypeF
    Bfloat    : LTypeF
    LFloat    : LTypeF
    LDouble   : LTypeF
    FP128     : LTypeF
    X86_FP80  : LTypeF
    PPC_FP128 : LTypeF
  public export
  -- TODO: Target types
  data LType : Type where
    LPtr : LType
    LPtrAddr : AddressSpace -> LType
    LVoid : LType
    LFun : LType -> List LType -> LType
    LFunVarArg : LType -> List LType -> LType -> LType
    LOpaque : LType 
    LInt : Int -> LType
    LFloating : LTypeF -> LType
    LX86_AMX : LType
    LVector : Int -> LType -> LType
    LVectorScale : Int -> LType -> LType
    LLabel : LType
    LToken : LType
    LArray : Int -> LType -> LType
    LStruct : List LType -> LType
    LPackedStruct : List LType -> LType
    LMetadata :  LType
public export
record WithType a where
  constructor MkWithType
  tpe : LType
  value : a


public export
data Attribute : Type where
  ZeroExt : Attribute
  SignExt : Attribute
  NoExt : Attribute
  ByVal : LType -> Attribute
  ByRef : LType -> Attribute
  Preallocated : LType -> Attribute
  Inalloca : LType -> Attribute
  SRet : LType -> Attribute
  Align : Nat -> Attribute
  NoAlias : Attribute
  NoFree : Attribute
  Nest : Attribute
  Returned : Attribute
  NoNull : Attribute
  Dereferenceable : Nat -> Attribute
  DereferenceableOrNull : Nat -> Attribute
  SwiftSelf : Attribute
  SwiftAsync : Attribute
  SwiftError : Attribute
  ImmArg : Attribute
  NoUndef : Attribute
  AlignStack : Nat -> Attribute
  AllocAlign : Attribute
  AllocPtr : Attribute
  ReadNone : Attribute
  ReadOnly : Attribute
  WriteOnly : Attribute
  Writeable : Attribute
  DeadOnUnwind : Attribute
  DeadOnReturn : Attribute
  --AllocFamily : String -> Attribute 
  --AllocKind : String -> Attribute
  OtherAttribute : String -> Attribute
  --TODO: Finish


public export
record FunctionArgSpec where 
  constructor MkFunctionArgSpec
  type : LType
  attrs : List Attribute
  name : Maybe String 
public export 
data Metadata : Type where -- TODO:
  ||| A metadata value
  MetadataNode : String -> Metadata
  ||| A metadata node
  MetadataTuple : List Metadata -> Metadata
||| The inline asembly
public export 
data Assembly : Type where 
  BasicAsm : String -> Assembly
-- TODO: Captures, nofp, inits, range, func atrributes
public export
data LConst : Type where 
  LInt : Int -> LConst
  LFloat : String -> LConst
  LBool : Bool -> LConst 
  LNull : LConst
  LToken : LConst 
  LString : String -> LConst
  LArray : List (WithType LConst) -> LConst
  LVector : List (WithType LConst) -> LConst
  LStruct : List (WithType LConst) -> LConst
  LUndefined : LConst
  LPoison : LConst
  LZero : LConst
  LMetadata : Metadata -> LConst
  LPtr : Name -> LConst
  -- TODO: Basic block, dso-local, pointer auth, constant expression
  
namespace LExpr 
  public export
  data LExpr : Type where 
    LConstE : LConst -> LExpr

public export
data LTag : Type where 
  ||| A custom tag 
  CTag : String -> LTag
public export
record SymbolInfo where 
  constructor MkSymbolInfo 
  linkage : Maybe Linkage
  preemption : Maybe Preemption
  visibility : Maybe Visibility
  dllStorage : Maybe DLLStorage

Label = LExpr
public export 
data FastMathFlag : Type where
  FFast : FastMathFlag
  NoNaNs : FastMathFlag
  NoInfs : FastMathFlag
  NoSignedZeros : FastMathFlag

public export 
FastMath : Type 
FastMath = List FastMathFlag
  

