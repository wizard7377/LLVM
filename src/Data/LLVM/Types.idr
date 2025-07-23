module Data.LLVM.Types
  
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
public export
data ParameterAttr = ZeroExt -- TODO: 
-- PARAM attributes
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

namespace LConst 
  public export
  data LConst : Type where 
    LInt : Int -> LConst
    LFloat : String -> LConst
    LBool : Bool -> LConst 
    LNull : LConst
    LToken : LConst 
    LString : String -> LConst
  
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
record FunctionArgSpec where 
  constructor MkFunctionArgSpec
  name : String 
  type : LType
  attrs : List ParameterAttr

|||define [linkage] [PreemptionSpecifier] [visibility] [DLLStorageClass]
|||       [cconv] [ret attrs]
|||       <ResultType> @<FunctionName> ([argument list])
|||       [(unnamed_addr|local_unnamed_addr)] [AddrSpace] [fn Attrs]
|||       [section "name"] [partition "name"] [comdat [($name)]] [align N]
|||       [gc] [prefix Constant] [prologue Constant] [personality Constant]
|||       (!name !N)* { ... }
public export 
record FunctionBody where 
  constructor MkFunctionBody
  
public export
record FunctionDef where
  constructor MkFunctionDef
  name : String 
  symbolInfo : SymbolInfo
  callingConvention : Maybe CallingConvention
  returnAttrs : List ParameterAttr
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
  returnAttrs : List ParameterAttr
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
  
-- TODO: Comdats
-- TODO: fin param attributes
-- TODO: Bundles

Label = LExpr
public export 
data FastMath = FFast | NoNaNs | NoInfs | NoSignedZeros

public export
record WithType a where
  constructor MkWithType
  tpe : LType
  value : a