module Data.LLVM.Ops 

import Data.LLVM.Types
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
    returnAttrs : List ParameterAttr
    addressSpace : Maybe AddressSpace
    tpe : LType 
    fnval : LExpr
    args : List LExpr
    --fnAttrs : List ?
    --operandBundles : ?
    normal : Label
    unwind : Label
record BrCall where 
    constructor MkBrCall
    cc : Maybe CallingConvention
    returnAttrs : List ParameterAttr
    addressSpace : Maybe AddressSpace
    tpe : LType 
    fnval : LExpr
    args : List LExpr
    --fnAttrs : List ?
    --operandBundles : ?
    fallthrough : Label
    indirect: List Label

record CatchSwitch where 
    constructor MkCatchSwitch
    name : Name
    parent: Maybe Label 
    handlers : List Label 
    unwind: Maybe Label

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

data Wrapping = NoSigned | NoUnsigned | NoSignedUnsigned
data UnaryOpcode = 
    FNeg

data BinaryOpcode = 
    Add
    | AddWrap Wrapping
    | FAdd (List FastMath)
    | Sub
    | SubWrap Wrapping
    | FSub (List FastMath)
    | Mul
    | MulWrap Wrapping
    | FMul (List FastMath)
    | UDiv
    | UDivExact
    | SDiv
    | SDivExact
    | FDiv (List FastMath)
    | URem  
    | SRem 
    | FRem (List FastMath)
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

data VectorOpcode : Type where
    InsertElement : WithType LExpr -> WithType LExpr -> WithType LExpr -> VectorOpcode
    ShuffleVector : WithType LExpr -> WithType LExpr -> WithType LExpr -> VectorOpcode
data AggregateOpcode : Type where
    ExtractValue : WithType LExpr -> Nat -> AggregateOpcode
    InsertValue : WithType LExpr -> WithType LExpr -> Nat -> AggregateOpcode

data MemoryOp : Type where
    -- TODO: Inalloca?
    Alloc : LType -> Maybe (WithType Nat) -> Maybe Nat -> Maybe AddressSpace -> MemoryOp
    -- TODO: Load, because im not dealing with that right now
data Operation : Type where 
    TerminatorOp : Terminator -> Operation
    UnaryOp : UnaryOpcode -> LType -> LExpr -> Operation
    BinaryOp : BinaryOpcode -> LType -> LExpr -> LExpr -> Operation
    VectorOp : VectorOpcode -> Operation
    AggregateOp : AggregateOpcode -> Operation