module Data.LLVM.Ops 

import Data.LLVM.Core

||| Case branch for switch instructions.
||| Models individual cases in LLVM IR switch statements like:
||| ```llvm
||| switch i32 %val, label %default [
||||   i32 1, label %case1
||||   i32 2, label %case2
||| ]
||| ```
public export 
record CaseBranch where 
    constructor MkCaseBranch
    ||| Type of the case value
    tpe : LType
    ||| Case value to match
    value : LExpr
    ||| Target label for this case
    label : Label
||| Invoke instruction call specification.
||| Models LLVM IR invoke instructions like:
||| ```llvm
||| %result = invoke fastcc i32 @function(i32 %arg) 
||||           to label %normal unwind label %exception
||| ```
public export 
record InvokeCall where 
    constructor MkInvokeCall
    ||| Calling convention (fastcc, coldcc, etc.)
    cc : Maybe CallingConvention
    ||| Return value attributes
    returnAttrs : List Attribute
    ||| Address space for the function pointer
    addressSpace : Maybe AddressSpace
    ||| Function type
    tpe : LType 
    ||| Function value or pointer
    fnval : LExpr
    ||| Function arguments
    args : List LExpr
    --fnAttrs : List ?
    --operandBundles : ?
    ||| Normal execution continuation label
    normal : Label
    ||| Exception unwind label
    unwind : Label
||| Call branch instruction specification.
||| Models LLVM IR callbr instructions like:
||| ```llvm
||| callbr void asm "", "r,!i"(i32 %x) 
||||        to label %fallthrough [label %indirect1, label %indirect2]
||| ```
public export
record BrCall where 
    constructor MkBrCall
    ||| Calling convention
    cc : Maybe CallingConvention
    ||| Return value attributes
    returnAttrs : List Attribute
    ||| Address space for the function pointer
    addressSpace : Maybe AddressSpace
    ||| Function type
    tpe : LType 
    ||| Function value or pointer
    fnval : LExpr
    ||| Function arguments
    args : List LExpr
    --fnAttrs : List ?
    --operandBundles : ?
    ||| Fallthrough label for normal execution
    fallthrough : Label
    ||| Possible indirect branch targets
    indirect: List Label
||| Catch switch instruction specification.
||| Models LLVM IR catchswitch instructions like:
||| ```llvm
||| %cs = catchswitch within none [label %handler1, label %handler2] 
||||       unwind to caller
||| ```
public export
record CatchSwitch where 
    constructor MkCatchSwitch
    ||| Name of the catchswitch result
    name : Name
    ||| Parent scope (within clause)
    parent: Maybe Label 
    ||| List of exception handler labels
    handlers : List Label 
    ||| Unwind destination (to caller or label)
    unwind: Maybe Label
||| Tail call modifiers for function calls.
||| Models LLVM IR tail call prefixes like:
||| ```llvm
||| %result = tail call i32 @func(i32 %arg)
||| %result = musttail call i32 @func(i32 %arg)
||| %result = notail call i32 @func(i32 %arg)
||| ```
public export
data TailCall : Type where 
    ||| Regular tail call optimization hint
    Tail : TailCall
    ||| Mandatory tail call (must be optimized)
    MustTail : TailCall
    ||| Disable tail call optimization
    NoTail : TailCall
||| Function call instruction specification.
||| Models LLVM IR call instructions like:
||| ```llvm
||| %result = call i32 @function(i32 %arg1, i8* %arg2)
||| %result = tail call fastcc float @fastfunc(float %x)
||| call void @procedure()
||| ```
public export
record FnCall where 
    constructor MkFnCall
    ||| Tail call modifier
    tail: TailCall
    ||| Fast math flags for floating point operations
    fastMath : FastMath
    ||| Calling convention
    cc : Maybe CallingConvention
    ||| Return value attributes
    returnAttrs : List Attribute
    ||| Address space for the function pointer
    addressSpace : Maybe AddressSpace
    ||| Function type
    tpe : LType 
    ||| Function value or pointer
    fnval : LExpr
    ||| Function arguments with their types
    args : List (WithType LExpr)
    ||| Function attributes
    fnAttrs : List Attribute
    --operandBundles : ?
||| Terminator instructions that end basic blocks.
||| Models LLVM IR terminator instructions like:
||| ```llvm
||| ret void
||| ret i32 %value
||| br label %target
||| br i1 %cond, label %true, label %false
||| switch i32 %val, label %default [ i32 0, label %case0 ]
||| ```
public export
data Terminator : Type where
    ||| Return void from function
    RetVoid : Terminator
    ||| Return value from function
    Ret : LType -> LExpr -> Terminator
    ||| Conditional branch (br i1 %cond, label %true, label %false)
    CondBr : LExpr -> LExpr -> LExpr -> Terminator
    ||| Unconditional branch (br label %target)
    JumpBr : LExpr -> Terminator
    ||| Switch statement with multiple cases
    Switch : LType -> LExpr -> Name -> List CaseBranch -> Terminator
    ||| Indirect branch through computed address
    IndirectBr : LExpr -> List LExpr -> Terminator
    ||| Invoke instruction (function call with exception handling)
    Invoke : InvokeCall -> Terminator
    ||| Call branch instruction (inline assembly with possible branches)
    CallBR : BrCall -> Terminator
    ||| Resume exception propagation
    Resume : LType -> LExpr -> Terminator
    ||| Unreachable code marker
    Unreachable : Terminator
    ||| Catch switch for exception handling
    CatchSwitchOp : CatchSwitch -> Terminator
    ||| Return from catch handler
    CatchRet : LExpr -> Label -> Terminator
    ||| Return from cleanup to caller
    CleanupRetCaller : LExpr -> Terminator
    ||| Return from cleanup to specific label
    CleanupRet : LExpr -> Label -> Terminator

||| Integer comparison predicates for icmp instruction.
||| Models LLVM IR icmp comparisons like:
||| ```llvm
||| %result = icmp eq i32 %a, %b
||| %result = icmp slt i32 %x, %y
||| ```
public export
data Comparison : Type where
    ||| Equal (==)
    CEq   : Comparison
    ||| Not equal (!=)
    CNe   : Comparison
    ||| Unsigned greater than (>)
    CUGt  : Comparison
    ||| Unsigned greater than or equal (>=)
    CUGe  : Comparison
    ||| Unsigned less than (<)
    CULt  : Comparison
    ||| Unsigned less than or equal (<=)
    CULe  : Comparison
    ||| Signed greater than (>)
    CSGt  : Comparison
    ||| Signed greater than or equal (>=)
    CSGe  : Comparison
    ||| Signed less than (<)
    CSLt  : Comparison
    ||| Signed less than or equal (<=)
    CSLe  : Comparison

||| Integer overflow wrapping behavior.
||| Models LLVM IR overflow flags like:
||| ```llvm
||| %result = add nuw i32 %a, %b     ; no unsigned wrap
||| %result = add nsw i32 %a, %b     ; no signed wrap  
||| %result = add nuw nsw i32 %a, %b ; no wrap at all
||| ```
public export
data Wrapping : Type where
    ||| No signed wrap (nsw)
    NoSigned : Wrapping
    ||| No unsigned wrap (nuw)
    NoUnsigned : Wrapping
    ||| No signed or unsigned wrap (nuw nsw)
    NoSignedUnsigned : Wrapping

||| Unary operation opcodes.
||| Models LLVM IR unary instructions like:
||| ```llvm
||| %result = fneg float %x
||| ```
public export
data UnaryOpcode : Type where
    ||| Floating point negation
    FNeg : UnaryOpcode

||| Binary operation opcodes for arithmetic and logical operations.
||| Models LLVM IR binary instructions like:
||| ```llvm
||| %result = add i32 %a, %b
||| %result = fadd float %x, %y
||| %result = and i1 %p, %q
||| %result = shl i32 %val, 2
||| ```
|||All the simple binary opcodes
public export
data BinaryOpcode : Type where
    ||| Integer addition
    Add : BinaryOpcode
    ||| Integer addition with wrap flags
    AddWrap : Wrapping -> BinaryOpcode
    ||| Floating point addition with fast math
    FAdd : FastMath -> BinaryOpcode
    ||| Integer subtraction
    Sub : BinaryOpcode
    ||| Integer subtraction with wrap flags
    SubWrap : Wrapping -> BinaryOpcode
    ||| Floating point subtraction with fast math
    FSub : FastMath -> BinaryOpcode
    ||| Integer multiplication
    Mul : BinaryOpcode
    ||| Integer multiplication with wrap flags
    MulWrap : Wrapping -> BinaryOpcode
    ||| Floating point multiplication with fast math
    FMul : FastMath -> BinaryOpcode
    ||| Unsigned integer division
    UDiv : BinaryOpcode
    ||| Exact unsigned integer division
    UDivExact : BinaryOpcode
    ||| Signed integer division
    SDiv : BinaryOpcode
    ||| Exact signed integer division
    SDivExact : BinaryOpcode
    ||| Floating point division with fast math
    FDiv : FastMath -> BinaryOpcode
    ||| Unsigned integer remainder
    URem : BinaryOpcode
    ||| Signed integer remainder
    SRem : BinaryOpcode
    ||| Floating point remainder with fast math
    FRem : FastMath -> BinaryOpcode
    ||| Shift left
    Shl : BinaryOpcode
    ||| Shift left with wrap flags
    ShlWrap : Wrapping -> BinaryOpcode
    ||| Logical shift right
    LShr : BinaryOpcode
    ||| Exact logical shift right
    LShrExact : BinaryOpcode
    ||| Arithmetic shift right
    AShr : BinaryOpcode
    ||| Exact arithmetic shift right
    AShrExact : BinaryOpcode
    ||| Bitwise AND
    And : BinaryOpcode
    ||| Bitwise OR
    Or : BinaryOpcode
    ||| Disjoint bitwise OR (operands have no common set bits)
    DisjointOr : BinaryOpcode
    ||| Bitwise XOR
    Xor : BinaryOpcode
||| Vector operation opcodes.
||| Models LLVM IR vector manipulation instructions like:
||| ```llvm
||| %result = insertelement <4 x i32> %vec, i32 %val, i32 0
||| %result = extractelement <4 x i32> %vec, i32 2
||| %result = shufflevector <4 x i32> %v1, <4 x i32> %v2, <4 x i32> <i32 0, i32 5, i32 2, i32 7>
||| ```
public export
data VectorOpcode : Type where
    ||| Insert element into vector at specified index
    InsertElement : WithType LExpr -> WithType LExpr -> WithType LExpr -> VectorOpcode
    ||| Shuffle two vectors according to mask
    ShuffleVector : WithType LExpr -> WithType LExpr -> WithType LExpr -> VectorOpcode
    ||| Extract element from vector at specified index
    ExtractElement : WithType LExpr -> WithType LExpr -> VectorOpcode

||| Aggregate operation opcodes for structs and arrays.
||| Models LLVM IR aggregate manipulation instructions like:
||| ```llvm
||| %result = extractvalue {i32, float} %agg, 0
||| %result = insertvalue {i32, float} %agg, i32 42, 0
||| ```
public export
data AggregateOpcode : Type where
    ||| Extract value from aggregate at specified index
    ExtractValue : WithType LExpr -> Nat -> AggregateOpcode
    ||| Insert value into aggregate at specified index
    InsertValue : WithType LExpr -> WithType LExpr -> Nat -> AggregateOpcode
||| Type conversion operation opcodes.
||| Models LLVM IR conversion instructions like:
||| ```llvm
||| %result = trunc i32 %val to i16
||| %result = zext i16 %val to i32
||| %result = bitcast i8* %ptr to i32*
||| %result = addrspacecast i8* %ptr to i8 addrspace(1)*
||| ```
public export
data ConversionOpCode : Type where 
    ||| Truncate integer to smaller type
    Trunc : Wrapping -> ConversionOpCode
    ||| Zero extend integer to larger type
    ZExt : ConversionOpCode 
    ||| Sign extend integer to larger type
    SExt : ConversionOpCode
    ||| Truncate floating point to smaller precision
    FPTrunc : FastMath -> ConversionOpCode
    ||| Extend floating point to larger precision
    FPExt : FastMath -> ConversionOpCode
    ||| Convert floating point to unsigned integer
    FPToUi : ConversionOpCode
    ||| Convert floating point to signed integer
    FPToSi : ConversionOpCode
    ||| Convert unsigned integer to floating point
    UiToFP : ConversionOpCode
    ||| Convert signed integer to floating point
    SiToFP : ConversionOpCode
    ||| Convert pointer to integer
    PtrToInt : ConversionOpCode
    -- TODO: IntToPtr : ConversionOpCode
    ||| Bitwise cast between types of same size
    BitCast : ConversionOpCode
    ||| Convert pointer between address spaces
    AddrSpaceCast : AddressSpace -> ConversionOpCode
||| Comparison operation opcodes.
||| Models LLVM IR comparison instructions like:
||| ```llvm
||| %result = icmp eq i32 %a, %b
||| %result = fcmp olt float %x, %y
||| %result = fcmp true float %a, %b    ; always true
||| ```
public export
data CompareOpcode : Type where 
    ||| Integer comparison
    ICmp : Comparison -> CompareOpcode
    ||| Integer comparison with sign information
    ICmpSign : Comparison -> CompareOpcode
    ||| Ordered floating point comparison
    FCmpOrd : FastMath -> Comparison -> CompareOpcode
    ||| Unordered floating point comparison
    FCmpUnOrd : FastMath -> Comparison -> CompareOpcode
    ||| Always false comparison
    FCmpFalse : CompareOpcode
    ||| Always true comparison
    FCmpTrue : CompareOpcode

||| Miscellaneous operation opcodes.
||| Models LLVM IR miscellaneous instructions like:
||| ```llvm
||| %result = phi i32 [ %val1, %bb1 ], [ %val2, %bb2 ]
||| %result = select i1 %cond, i32 %true_val, i32 %false_val
||| %result = freeze i32 %val
||| ```
public export
data MiscOpcode : Type where 
    ||| PHI node for SSA form
    Phi : LType -> List (LExpr, Label) -> MiscOpcode
    ||| Conditional select instruction
    Select : FastMath -> WithType LExpr -> WithType LExpr -> WithType LExpr -> MiscOpcode
    ||| Freeze instruction (converts poison to undef)
    Freeze : WithType LExpr -> MiscOpcode
    ||| Function call operation
    FnCallOp : FnCall -> MiscOpcode
    -- TODO: VaArg : 
    -- TODO: LandingPad, CatchPad, CleanUpPad


public export
data AtomicOrder : Type where
    ||| No atomic ordering
    Unordered : AtomicOrder
    ||| Monotonic atomic ordering
    Monotonic : AtomicOrder
    ||| Acquire atomic ordering
    Acquire : AtomicOrder
    ||| Release atomic ordering
    Release : AtomicOrder
    ||| Acquire-release atomic ordering
    AcquireRelease : AtomicOrder
    ||| Sequentially consistent atomic ordering
    SequentiallyConsistent : AtomicOrder
||| Memory operation opcodes for memory allocation and access.
||| Models LLVM IR memory instructions like:
||| ```llvm
||| %ptr = alloca i32, align 4
||| %ptr = alloca i32, i32 %count, align 8
||| %ptr = alloca i32, align 4, addrspace(1)
||| ```
public export
data MemoryOpcode : Type where
    -- TODO: Inalloca?
    ||| <result> = alloca [inalloca] <type> [, <ty> <NumElements>] [, align <alignment>] [, addrspace(<num>)]     ; yields type addrspace(num)*:result
    ||| Stack allocation instruction
    Alloc : LType -> Maybe (WithType Nat) -> Maybe Nat -> Maybe AddressSpace -> MemoryOpcode
    -- TODO: Load, because im not dealing with that right now
    ||| Many of the args are written as Bools, as the spec has them as useless metadata
    LoadRegular : 
        (volatile : Bool) ->
        (tpe : LType) ->
        (address : LExpr) -> 
        (align : Maybe Nat) ->
        (nonTemporal : Bool) -> 
        (invariantLoad : Bool) ->
        (invariantGroup : Bool) ->
        (nonNull : Bool) ->
        (dereferenceable : Maybe Metadata) ->
        (dereferenceableOrNull : Maybe Metadata) ->
        (aligned : Maybe Nat) ->
        (noUndef : Bool) ->
        MemoryOpcode
    LoadAtomic : 
        (volatile : Bool) ->
        (tpe : LType) ->
        (address : LExpr) -> 
        (scope : Maybe String) -> 
        (ordering : Maybe AtomicOrder) ->
        (align : Maybe Nat) ->
        (nontemporal : Bool) -> 
        (invariantGroup : Bool) ->
        MemoryOpcode

    StoreRegular : 
        (volatile : Bool) ->
        (tpe : WithType LExpr) ->
        (address : LExpr) -> 
        (align : Maybe Nat) ->
        (nonTemporal : Bool) -> 
        (invariantGroup : Bool) ->
        MemoryOpcode
    StoreAtomic : 
        (volatile : Bool) ->
        (tpe : WithType LExpr) ->
        (address : LExpr) -> 
        (scope : Maybe String) -> 
        (ordering : Maybe AtomicOrder) ->
        (align : Maybe Nat) ->
        (invariantGroup : Bool) ->
        MemoryOpcode
    Fence : 
        (scope : Maybe String) ->
        (ordering : Maybe AtomicOrder) ->
        MemoryOpcode
    -- TODO: Cmpxchg, atomicrmw, etc.

public export
data CatchClause : Type where
    ||| Catch clause for landing pad instructions
    Catching : LType -> LConst -> CatchClause
    ||| Filter clause for landing pad instructions
    Filtering : LType -> LConst -> CatchClause
public export 
data ExceptOpcode : Type where 
    LandingPad : LType -> List CatchClause -> ExceptOpcode
    LandingPadCleanup : LType -> List CatchClause -> ExceptOpcode
    CatchPad : Name -> LConst -> ExceptOpcode
    CleanupPad : Name -> LConst -> ExceptOpcode
||| LLVM operations categorized by type.
||| Represents all possible LLVM IR operations.
public export
data LOperation : Type where 
    ||| Terminator instruction (ends basic blocks)
    TerminatorOp : Terminator -> LOperation
    ||| Unary operation
    UnaryOp : UnaryOpcode -> LType -> LExpr -> LOperation
    ||| Binary operation
    BinaryOp : BinaryOpcode -> LType -> LExpr -> LExpr -> LOperation
    ||| Vector operation
    VectorOp : VectorOpcode -> LOperation
    ||| Aggregate operation
    AggregateOp : AggregateOpcode -> LOperation
    ||| Type conversion operation
    ConversionOp : ConversionOpCode -> WithType LExpr -> LType -> LOperation
    ||| Miscellaneous operation
    MiscOp : MiscOpcode -> LOperation
    ||| Memory operation
    MemoryOp : MemoryOpcode -> LOperation
    ||| Exception handling operation 
    ExceptOp : ExceptOpcode -> LOperation

||| LLVM statements that can appear in basic blocks.
||| Models different forms of LLVM IR statements like:
||| ```llvm
||| %result = add i32 %a, %b        ; targeted assignment
||| call void @function()           ; discarded result
||| entry:                          ; basic block label
||| ```
public export
data LStatement : Type where 
    ||| Operation with named result (%name = operation)
    Targeted : Name -> LOperation -> LStatement
    ||| Operation with discarded result
    Discarded : LOperation -> LStatement
    ||| Basic block label
    Labelled : String -> LStatement