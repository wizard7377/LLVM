module Data.LLVM.Ops 

import Data.LLVM.Core
public export 
record CaseBranch where 
    constructor MkCaseBranch
    tpe : LType
    value : LExpr
    label : Label
public export 
record InvokeCall where 
    constructor MkInvokeCall
    cc : Maybe CallingConvention
    returnAttrs : List Attribute
    addressSpace : Maybe AddressSpace
    tpe : LType 
    fnval : LExpr
    args : List LExpr
    --fnAttrs : List ?
    --operandBundles : ?
    normal : Label
    unwind : Label
public export
record BrCall where 
    constructor MkBrCall
    cc : Maybe CallingConvention
    returnAttrs : List Attribute
    addressSpace : Maybe AddressSpace
    tpe : LType 
    fnval : LExpr
    args : List LExpr
    --fnAttrs : List ?
    --operandBundles : ?
    fallthrough : Label
    indirect: List Label
public export
record CatchSwitch where 
    constructor MkCatchSwitch
    name : Name
    parent: Maybe Label 
    handlers : List Label 
    unwind: Maybe Label
public export
data TailCall = 
    Tail 
    | MustTail
    | NoTail
public export
record FnCall where 
    constructor MkFnCall
    tail: TailCall
    fastMath : FastMath

    cc : Maybe CallingConvention
    returnAttrs : List Attribute
    addressSpace : Maybe AddressSpace
    tpe : LType 
    fnval : LExpr
    args : List LExpr
    --fnAttrs : List ?
    --operandBundles : ?
public export 
data Terminator = 
    RetVoid 
    | Ret LType LExpr
    | CondBr LExpr LExpr LExpr
    | JumpBr LExpr 
    | Switch LType LExpr Name (List CaseBranch)
    | IndirectBr LExpr (List LExpr)
    | Invoke InvokeCall
    | CallBR BrCall
    | Resume LType LExpr
    | Unreachable
    | CatchSwitchOp CatchSwitch 
    | CatchRet LExpr Label
    | CleanupRetCaller LExpr 
    | CleanupRet LExpr Label

public export
data Comparison = CEq | CNe | CUGt | CUGe | CULt | CULe | CSGt | CSGe | CSLt | CSLe
public export
data Wrapping = NoSigned | NoUnsigned | NoSignedUnsigned
public export
data UnaryOpcode = 
    FNeg

|||All the simple binary opcodes
public export
data BinaryOpcode = 
    Add
    | AddWrap Wrapping
    | FAdd (FastMath)
    | Sub
    | SubWrap Wrapping
    | FSub (FastMath)
    | Mul
    | MulWrap Wrapping
    | FMul (FastMath)
    | UDiv
    | UDivExact
    | SDiv
    | SDivExact
    | FDiv (FastMath)
    | URem  
    | SRem 
    | FRem (FastMath)
    | Shl
    | ShlWrap Wrapping
    | LShr
    | LShrExact 
    | AShr
    | AShrExact
    | And 
    | Or 
    | DisjointOr 
    | Xor
public export
data VectorOpcode : Type where
    InsertElement : WithType LExpr -> WithType LExpr -> WithType LExpr -> VectorOpcode
    ShuffleVector : WithType LExpr -> WithType LExpr -> WithType LExpr -> VectorOpcode
    ExtractElement : WithType LExpr -> WithType LExpr -> VectorOpcode
public export
data AggregateOpcode : Type where
    ExtractValue : WithType LExpr -> Nat -> AggregateOpcode
    InsertValue : WithType LExpr -> WithType LExpr -> Nat -> AggregateOpcode
public export
data ConversionOpCode : Type where 
    Trunc : Wrapping -> ConversionOpCode
    ZExt : ConversionOpCode 
    SExt : ConversionOpCode
    FPTrunc : FastMath -> ConversionOpCode
    FPExt : FastMath -> ConversionOpCode
    FPToUi : ConversionOpCode
    FPToSi : ConversionOpCode
    UiToFP : ConversionOpCode
    SiToFP : ConversionOpCode
    PtrToInt : ConversionOpCode
    -- TODO: IntToPtr : ConversionOpCode
    BitCast : ConversionOpCode
    AddrSpaceCast : AddressSpace -> ConversionOpCode
public export
data CompareOpcode : Type where 
    ICmp : Comparison -> CompareOpcode
    ICmpSign : Comparison -> CompareOpcode
    FCmpOrd : FastMath -> Comparison -> CompareOpcode
    FCmpUnOrd : FastMath -> Comparison -> CompareOpcode
    FCmpFalse : CompareOpcode
    FCmpTrue : CompareOpcode

public export
data MiscOp : Type where 
    Phi : LType -> List (LExpr, Label) -> MiscOp
    Select : FastMath -> WithType LExpr -> LExpr -> WithType LExpr -> WithType LExpr -> MiscOp
    Freeze : WithType LExpr -> MiscOp
    FnCallOp : FnCall -> MiscOp
    -- TODO: VaArg : 
    -- TODO: LandingPad, CatchPad, CleanUpPad

public export
data MemoryOp : Type where
    -- TODO: Inalloca?
    Alloc : LType -> Maybe (WithType Nat) -> Maybe Nat -> Maybe AddressSpace -> MemoryOp
    -- TODO: Load, because im not dealing with that right now

public export
data LOperation : Type where 
    TerminatorOp : Terminator -> LOperation
    UnaryOp : UnaryOpcode -> LType -> LExpr -> LOperation
    BinaryOp : BinaryOpcode -> LType -> LExpr -> LExpr -> LOperation
    VectorOp : VectorOpcode -> LOperation
    AggregateOp : AggregateOpcode -> LOperation
    ConversionOp : ConversionOpCode -> WithType LExpr -> LExpr -> LOperation

public export
data LStatement : Type where 
    Targeted : Name -> LOperation -> LStatement
    Discarded : LOperation -> LStatement
    Labelled : Label -> LStatement