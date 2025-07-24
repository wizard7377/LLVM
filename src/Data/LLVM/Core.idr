module Data.LLVM.Core
  

public export
data Linkage = Private | Internal | Available | LinkOnce | Weak | Common | Appending | ExternWeak | LinkOnceODR | WeakODR | External
public export
data CallingConvention = C | Fast | Cold | GHC | CC11 | AnyReg | PreserveMost | PreserveAll | PreserveNone | CxxFastTL | Tail | Swift | SwiftTail | CFGuardCheck | CustomCC Int 
public export
data Visibility = Default | Hidden | Protected
public export
data DLLStorage = DLLExport | DLLImport
public export
data ThreadLocality = LocalDynamic | InitialExec | LocalExec 
public export
data Preemption = Preemptible | NonPreemptible
public export
data AddressInfo = UnnamedGlobal | UnamedLocal

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
  ||| User defined
  CustomN : String -> Name

namespace LType 
  public export 
  data LTypeF = Half | Bfloat | LFloat | LDouble | FP128 | X86_FP80 | PPC_FP128
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
data Attribute = ZeroExt | SignExt | NoExt | ByVal LType | ByRef LType | Preallocated LType | Inalloca LType | SRet LType | Align Nat | NoAlias | NoFree | Nest | Returned | NoNull | Dereferenceable Nat | DereferenceableOrNull Nat | SwiftSelf | SwiftAsync | SwiftError | ImmArg | NoUndef | AlignStack Nat | AllocAlign | AllocPtr | ReadNone | ReadOnly | WriteOnly | Writeable  | DeadOnUnwind | DeadOnReturn | NamedAttribute String
public export
record FunctionArgSpec where 
  constructor MkFunctionArgSpec
  name : String 
  type : LType
  attrs : List Attribute
public export 
data Metadata : Type where -- TODO:
  ||| A metadata value
  MetadataValue : String -> Metadata
  ||| A metadata node
  MetadataNode : List Metadata -> Metadata
  ||| A metadata attachment
  MetadataAttachment : String -> Metadata -> Metadata
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
    LConst : LConst -> LExpr

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
data FastMathFlag = FFast | NoNaNs | NoInfs | NoSignedZeros

public export 
FastMath : Type 
FastMath = List FastMathFlag
  
