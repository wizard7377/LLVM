module Data.LLVM.IR.Ops 

import Data.LLVM.IR.Core

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
    value : LValue
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
    fnval : LValue
    ||| Function arguments
    args : List LValue
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
    fnval : LValue
    ||| Function arguments
    args : List LValue
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
    fnval : LValue
    ||| Function arguments with their types
    args : List (WithType LValue)
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
    Ret : LType -> LValue -> Terminator
    ||| Conditional branch (br i1 %cond, label %true, label %false)
    CondBr : LValue -> Label -> Label -> Terminator
    ||| Unconditional branch (br label %target)
    JumpBr : Label -> Terminator
    ||| Switch statement with multiple cases
    Switch : LType -> LValue -> Name -> List CaseBranch -> Terminator
    ||| Indirect branch through computed address
    IndirectBr : LValue -> List LValue -> Terminator
    ||| Invoke instruction (function call with exception handling)
    Invoke : InvokeCall -> Terminator
    ||| Call branch instruction (inline assembly with possible branches)
    CallBR : BrCall -> Terminator
    ||| Resume exception propagation
    Resume : LType -> LValue -> Terminator
    ||| Unreachable code marker
    Unreachable : Terminator
    ||| Catch switch for exception handling
    CatchSwitchOp : CatchSwitch -> Terminator
    ||| Return from catch handler
    CatchRet : LValue -> Label -> Terminator
    ||| Return from cleanup to caller
    CleanupRetCaller : LValue -> Terminator
    ||| Return from cleanup to specific label
    CleanupRet : LValue -> Label -> Terminator

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


public export
data CatchClause : Type where
    ||| Catch clause for landing pad instructions
    Catching : LType -> LValue -> CatchClause
    ||| Filter clause for landing pad instructions
    Filtering : LType -> LValue -> CatchClause



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

||| Miscellaneous operation opcodes.
||| Models LLVM IR miscellaneous instructions like:
||| ```llvm
||| %result = phi i32 [ %val1, %bb1 ], [ %val2, %bb2 ]
||| %result = select i1 %cond, i32 %true_val, i32 %false_val
||| %result = freeze i32 %val
||| ```
public export
data LExpr : Type where 
    ||| PHI node for SSA form
    Phi : LType -> List (LValue, Label) -> LExpr
    ||| Conditional select instruction
    Select : FastMath -> WithType LValue -> WithType LValue -> WithType LValue -> LExpr
    ||| Freeze instruction (converts poison to undef)
    Freeze : WithType LValue -> LExpr
    ||| Function call operation
    FnCallOp : FnCall -> LExpr
    -- [ ]: VaArg
    LandingPad : LType -> List CatchClause -> LExpr
    LandingPadCleanup : LType -> List CatchClause -> LExpr
    CatchPad : Name -> LValue -> LExpr
    CleanupPad : Name -> LValue -> LExpr
    ||| Memory operation opcodes for memory allocation and access.
    ||| Models LLVM IR memory instructions like:
    ||| ```llvm
    ||| %ptr = alloca i32, align 4
    ||| %ptr = alloca i32, i32 %count, align 8
    ||| %ptr = alloca i32, align 4, addrspace(1)
    ||| ```
    ||| Memory operation opcodes for memory allocation and access.
    ||| Models LLVM IR memory instructions like:
    ||| ```llvm
    ||| %ptr = alloca i32, align 4
    ||| %ptr = alloca i32, i32 %count, align 8
    ||| %ptr = alloca i32, align 4, addrspace(1)
    ||| ```
    -- TODO: Inalloca?
    ||| <result> = alloca [inalloca] <type> [, <ty> <NumElements>] [, align <alignment>] [, addrspace(<num>)]     ; yields type addrspace(num)*:result
    ||| Stack allocation instruction
    Alloc : LType -> Maybe (WithType Nat) -> Maybe Nat -> Maybe AddressSpace -> LExpr
    -- TODO: Load, because im not dealing with that right now
    ||| Many of the args are written as Bools, as the spec has them as useless metadata
    LoadRegular : 
        (volatile : Bool) ->
        (tpe : LType) ->
        (address : LValue) -> 
        (align : Maybe Nat) ->
        (nonTemporal : Bool) -> 
        (invariantLoad : Bool) ->
        (invariantGroup : Bool) ->
        (nonNull : Bool) ->
        (dereferenceable : Maybe Metadata) ->
        (dereferenceableOrNull : Maybe Metadata) ->
        (aligned : Maybe Nat) ->
        (noUndef : Bool) ->
        LExpr
    LoadAtomic : 
        (volatile : Bool) ->
        (tpe : LType) ->
        (address : LValue) -> 
        (scope : Maybe String) -> 
        (ordering : Maybe AtomicOrder) ->
        (align : Maybe Nat) ->
        (nontemporal : Bool) -> 
        (invariantGroup : Bool) ->
        LExpr

    StoreRegular : 
        (volatile : Bool) ->
        (tpe : WithType LValue) ->
        (address : LValue) -> 
        (align : Maybe Nat) ->
        (nonTemporal : Bool) -> 
        (invariantGroup : Bool) ->
        LExpr
    StoreAtomic : 
        (volatile : Bool) ->
        (tpe : WithType LValue) ->
        (address : LValue) -> 
        (scope : Maybe String) -> 
        (ordering : Maybe AtomicOrder) ->
        (align : Maybe Nat) ->
        (invariantGroup : Bool) ->
        LExpr
    Fence : 
        (scope : Maybe String) ->
        (ordering : Maybe AtomicOrder) ->
        LExpr
    CmpXChg : (weak : Bool) -> (volatile : Bool) -> LValue -> WithType LValue -> WithType LValue -> (syncscope : Maybe String) -> AtomicOrder -> AtomicOrder -> LExpr 
    
    -- TODO: Cmpxchg, atomicrmw, etc.
    ||| Unary operation opcodes.
    ||| Models LLVM IR unary instructions like:
    ||| ```llvm
    ||| %result = fneg float %x
    ||| ```

    ||| Floating point negation
    FNeg : LType -> LValue -> LExpr

    ||| Binary operation opcodes for arithmetic and logical operations.
    ||| Models LLVM IR binary instructions like:
    ||| ```llvm
    ||| %result = add i32 %a, %b
    ||| %result = fadd float %x, %y
    ||| %result = and i1 %p, %q
    ||| %result = shl i32 %val, 2
    ||| ```
    |||All the simple binary opcodes

    Add : LType -> LValue -> LValue -> LExpr
    AddWrap : Wrapping -> LType -> LValue -> LValue -> LExpr
    FAdd : FastMath -> LType -> LValue -> LValue -> LExpr
    Sub : LType -> LValue -> LValue -> LExpr
    SubWrap : Wrapping -> LType -> LValue -> LValue -> LExpr
    FSub : FastMath -> LType -> LValue -> LValue -> LExpr
    Mul : LType -> LValue -> LValue -> LExpr
    MulWrap : Wrapping -> LType -> LValue -> LValue -> LExpr
    FMul : FastMath -> LType -> LValue -> LValue -> LExpr
    UDiv : LType -> LValue -> LValue -> LExpr
    UDivExact : LType -> LValue -> LValue -> LExpr
    SDiv : LType -> LValue -> LValue -> LExpr
    SDivExact : LType -> LValue -> LValue -> LExpr
    FDiv : FastMath -> LType -> LValue -> LValue -> LExpr
    URem : LType -> LValue -> LValue -> LExpr
    SRem : LType -> LValue -> LValue -> LExpr
    FRem : FastMath -> LType -> LValue -> LValue -> LExpr
    Shl : LType -> LValue -> LValue -> LExpr
    ShlWrap : Wrapping -> LType -> LValue -> LValue -> LExpr
    LShr : LType -> LValue -> LValue -> LExpr
    LShrExact : LType -> LValue -> LValue -> LExpr
    AShr : LType -> LValue -> LValue -> LExpr
    AShrExact : LType -> LValue -> LValue -> LExpr
    And : LType -> LValue -> LValue -> LExpr
    Or : LType -> LValue -> LValue -> LExpr
    DisjointOr : LType -> LValue -> LValue -> LExpr
    Xor : LType -> LValue -> LValue -> LExpr
    ||| Vector operation opcodes.
    ||| Models LLVM IR vector manipulation instructions like:
    ||| ```llvm
    ||| %result = insertelement <4 x i32> %vec, i32 %val, i32 0
    ||| %result = extractelement <4 x i32> %vec, i32 2
    ||| %result = shufflevector <4 x i32> %v1, <4 x i32> %v2, <4 x i32> <i32 0, i32 5, i32 2, i32 7>
    ||| ```

    ||| Insert element into vector at specified index
    InsertElement : WithType LValue -> WithType LValue -> WithType LValue -> LExpr
    ||| Shuffle two vectors according to mask
    ShuffleVector : WithType LValue -> WithType LValue -> WithType LValue -> LExpr
    ||| Extract element from vector at specified index
    ExtractElement : WithType LValue -> WithType LValue -> LExpr

    ||| Aggregate operation opcodes for structs and arrays.
    ||| Models LLVM IR aggregate manipulation instructions like:
    ||| ```llvm
    ||| %result = extractvalue {i32, float} %agg, 0
    ||| %result = insertvalue {i32, float} %agg, i32 42, 0
    ||| ```

    ||| Extract value from aggregate at specified index
    ExtractValue : WithType LValue -> Nat -> LExpr
    ||| Insert value into aggregate at specified index
    InsertValue : WithType LValue -> WithType LValue -> Nat -> LExpr
    ||| Type conversion operation opcodes.
    ||| Models LLVM IR conversion instructions like:
    ||| ```llvm
    ||| %result = trunc i32 %val to i16
    ||| %result = zext i16 %val to i32
    ||| %result = bitcast i8* %ptr to i32*
    ||| %result = addrspacecast i8* %ptr to i8 addrspace(1)*
    ||| ```

    Trunc : Wrapping -> WithType LValue -> LType -> LExpr
    ZExt : WithType LValue -> LType -> LExpr
    SExt : WithType LValue -> LType -> LExpr
    FPTrunc : FastMath -> WithType LValue -> LType -> LExpr
    FPExt : FastMath -> WithType LValue -> LType -> LExpr
    FPToUi : WithType LValue -> LType -> LExpr
    FPToSi : WithType LValue -> LType -> LExpr
    UiToFP : WithType LValue -> LType -> LExpr
    SiToFP : WithType LValue -> LType -> LExpr
    PtrToInt : WithType LValue -> LType -> LExpr
    -- TODO: IntToPtr : LExpr
    BitCast : WithType LValue -> LType -> LExpr
    AddrSpaceCast : AddressSpace -> WithType LValue -> LType -> LExpr
    ||| Comparison operation opcodes.
    ||| Models LLVM IR comparison instructions like:
    ||| ```llvm
    ||| %result = icmp eq i32 %a, %b
    ||| %result = fcmp olt float %x, %y
    ||| %result = fcmp true float %a, %b    ; always true
    ||| ```

    ICmp : Comparison -> LType -> LValue -> LValue -> LExpr
    ICmpSign : Comparison -> LType -> LValue -> LValue -> LExpr
    FCmpOrd : FastMath -> Comparison -> LType -> LValue -> LValue -> LExpr
    FCmpUnOrd : FastMath -> Comparison -> LType -> LValue -> LValue -> LExpr
    FCmpFalse : LType -> LValue -> LValue -> LExpr
    FCmpTrue : LType -> LValue -> LValue -> LExpr


||| LLVM statements that can appear in basic blocks.
||| Models different forms of LLVM IR statements like:
||| ```llvm
||| %result = add i32 %a, %b        ; targeted assignment
||| call void @function()           ; discarded result
||| entry:                          ; basic block label
||| ```
public export
record LStatement where
    constructor MkLStatement
    ||| The name of the result variable (if any)
    target : Maybe Name
    ||| The instruction being executed
    instruction : LExpr
    ||| The metadata associated with the statement (if any)
    metadata : Annotation

||| Basic block 
public export 
record BasicBlock where 
    constructor MkBasicBlock
    ||| BasicBlock name (without label prefix)
    name : String
    ||| List of statements in the block
    statements : List LStatement
    ||| Terminator instruction that ends the block
    terminator : Terminator
    
