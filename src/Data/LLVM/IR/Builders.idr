||| LLVM IR builder functions for constructing LLVM IR programmatically.
|||
||| This module provides convenient builder functions for creating LLVM IR constructs
||| with sensible defaults and optional parameters. The builders cover:
|||
||| - **Terminator operations**: switch, invoke, unreachable, etc.
||| - **Arithmetic operations**: add, sub, mul, div, rem with overflow/exact flags
||| - **Bitwise operations**: and, or, xor, shift operations
||| - **Comparison operations**: icmp, fcmp with different predicates
||| - **Memory operations**: alloca, load, store with various flags
||| - **Type conversion**: trunc, zext, sext, bitcast, fp conversions
||| - **Vector operations**: insert/extract/shuffle elements
||| - **Aggregate operations**: insert/extract values from structs/arrays
||| - **Control flow**: phi nodes, select, freeze
||| - **Type shortcuts**: i1, i8, i16, i32, i64, ptr, float, double
||| - **Constant builders**: integers, booleans, strings, arrays, etc.
|||
||| All builder functions use default arguments where sensible to reduce verbosity
||| while still allowing full customization when needed.
module Data.LLVM.IR.Builders

--import Data.LLVM.Class
import Data.LLVM.IR.Core       
--import Data.LLVM.Write
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Program
import Data.LLVM.IR.Alias
export
emptyFunctionBody : FunctionBody
emptyFunctionBody = MkFunctionBody [] 

Semigroup FunctionBody where 
    (<+>) (MkFunctionBody stmts1) (MkFunctionBody stmts2) = MkFunctionBody (stmts1 ++ stmts2)
Monoid FunctionBody where 
    neutral = emptyFunctionBody
export 
emptyModule : LModule
emptyModule = MkLModule Nothing Nothing [] Nothing 

export 
emptySymbolInfo : SymbolInfo
emptySymbolInfo = MkSymbolInfo Nothing Nothing Nothing Nothing 
export
functionDef :  
    (name : String) ->
    {default emptySymbolInfo symbolInfo : SymbolInfo} ->
    {default Nothing callingConvention : Maybe CallingConvention} ->
    {default [] returnAttrs : List Attribute} ->
    (retType : LType) ->
    (args : List FunctionArgSpec) -> 
    {default Nothing addressInfo : Maybe AddressInfo} -> 
    {default Nothing addressSpace : Maybe AddressSpace} -> 
    {default [] fnAttributes : List Attribute} ->
    {default Nothing section : Maybe String} ->
    {default Nothing partition : Maybe String} ->
    {default Nothing comdat : Maybe Name} ->
    {default Nothing alignment : Maybe Int} ->
    {default Nothing gc : Maybe String} ->
    {default Nothing fprefix : Maybe LConst} ->
    {default Nothing prologue : Maybe LConst} ->
    {default Nothing personality : Maybe LConst} ->
    {default [] metadata : List Metadata} ->
    (body : FunctionBody) ->
    {default [] tags : List LTag} ->
    FunctionDef
functionDef name {symbolInfo} {callingConvention} {returnAttrs} retType args {addressInfo} {addressSpace} {fnAttributes} {section} {partition} {comdat} {alignment} {gc} {fprefix} {prologue} {personality} {metadata} body {tags} =
    MkFunctionDef
        name
        symbolInfo
        callingConvention
        returnAttrs
        retType
        args
        addressInfo
        addressSpace
        fnAttributes
        section
        partition
        comdat
        alignment
        gc
        fprefix
        prologue
        personality
        metadata
        body 
        tags
        

export 
functionDec : 
    (name : String) ->
    {default emptySymbolInfo symbolInfo : SymbolInfo} ->
    {default Nothing callingConvention : Maybe CallingConvention} ->
    {default [] returnAttrs : List Attribute} ->
    (retType : LType) -> 
    (args : List FunctionArgSpec) ->
    {default Nothing addressInfo : Maybe AddressInfo} ->
    {default Nothing alignment : Maybe Int} ->
    {default Nothing gc : Maybe String} ->
    {default Nothing fprefix : Maybe LConst} ->
    {default Nothing prologue : Maybe LConst} ->
    {default [] tags : List LTag} ->
    FunctionDec
functionDec name {symbolInfo} {callingConvention} {returnAttrs} retType args {addressInfo} {alignment} {gc} {fprefix} {prologue} {tags} =
    MkFunctionDec
        name
        symbolInfo
        callingConvention
        returnAttrs
        retType
        args
        addressInfo
        alignment
        gc
        fprefix
        prologue
        tags
export
||| Make a global variable definition with configurable options.
globalDef : 
    (name : String) ->
    {default emptySymbolInfo symbolInfo : SymbolInfo} ->
    {default Nothing threadLocality : Maybe ThreadLocality} ->
    {default Nothing addressInfo : Maybe AddressInfo} ->
    {default Nothing addressSpace : Maybe AddressSpace} ->
    {default Nothing externallyInitialized : Maybe Bool} ->
    {default False isConst : Bool} ->
    (ty : LType) ->
    {default Nothing init : Maybe LConst} ->
    {default [] tags : List LTag} ->
    GVarDef
globalDef name {symbolInfo} {threadLocality} {addressInfo} {addressSpace} {externallyInitialized} {isConst} ty {init} {tags} =
    MkGVarDef
        name
        symbolInfo
        threadLocality
        addressInfo 
        addressSpace 
        externallyInitialized 
        isConst
        ty
        init
        tags

export
alias : 
    (name : String) ->
    {default emptySymbolInfo symbolInfo : SymbolInfo} ->
    {default Nothing threadLocality : Maybe ThreadLocality} ->
    {default Nothing addressInfo : Maybe AddressInfo} ->
    (ty : LType) ->
    {default ty ty2 : LType} ->
    (target : String) ->
    {default [] tags : List LTag} ->
    Alias
alias name {symbolInfo} {threadLocality} {addressInfo} ty {ty2} target {tags} =
    MkAlias
        name
        symbolInfo
        threadLocality
        addressInfo
        ty 
        target 
        []
        -- TODO: Remaining types
export
||| Create a case branch for switch statements.
caseBranch : 
    (tpe : LType) ->
    (value : LExpr) ->
    (label : Label) -> 
    CaseBranch
caseBranch tpe value label = MkCaseBranch tpe value label



public export
withType : 
    (ty : LType) ->
    (expr : LExpr) ->
    WithType LExpr
withType ty expr = MkWithType ty expr

export
||| Create a boolean constant.
mkBool : Bool -> LConst
mkBool b = LBool b

export
||| Create a string constant.
mkString : String -> LConst
mkString s = LString s

export
||| Create a null pointer constant.
mkNull : LConst
mkNull = LNull

export
||| Create an undefined constant.
mkUndefined : LConst
mkUndefined = LUndefined

export
||| Create a floating point constant.
mkFloat : String -> LConst
mkFloat f = LFloat f

export
||| Create an array constant.
mkArray : List (WithType LConst) -> LConst
mkArray elems = LArray elems

export
||| Create a struct constant.
mkStruct : List (WithType LConst) -> LConst
mkStruct fields = LStruct fields

export
||| Create a vector constant.
mkVector : List (WithType LConst) -> LConst
mkVector elems = LVector elems

export
||| Create a function argument specification with optional attributes and name.
functionArg : 
    (ty : LType) ->
    {default [] attrs : List Attribute} ->
    {default Nothing name : Maybe String} ->
    FunctionArgSpec
functionArg ty {attrs} {name} = MkFunctionArgSpec ty attrs name

export
||| Create a function call with configurable options.
fnCall : 
    {default NoTail tail : TailCall} ->
    {default [] fastMath : FastMath} ->
    {default Nothing cc : Maybe CallingConvention} ->
    {default [] returnAttrs : List Attribute} ->
    {default Nothing addressSpace : Maybe AddressSpace} ->
    (tpe : LType) ->
    (fnval : LExpr) ->
    (args : List (WithType LExpr)) ->
    {default [] fnAttrs : List Attribute} ->
    FnCall
fnCall {tail} {fastMath} {cc} {returnAttrs} {addressSpace} tpe fnval args {fnAttrs} =
    MkFnCall tail fastMath cc returnAttrs addressSpace tpe fnval args fnAttrs

export
||| Create a simple function call with minimal arguments.
simpleFnCall : 
    (tpe : LType) ->
    (fnval : LExpr) ->
    (args : List (WithType LExpr)) ->
    FnCall
simpleFnCall tpe fnval args = fnCall tpe fnval args

export
||| Create an expression from a constant.
constExpr : LConst -> LExpr
constExpr c = LConstE c

export
||| Create a local variable reference.
local : String -> Name
local name = Local name

export
||| Create a global variable reference.
global : String -> Name
global name = Global name

export
||| Create a pointer expression from a name.
ptrExpr : Name -> LExpr
ptrExpr name = LConstE (LPtr name)

export
||| Create a local variable pointer expression.
localPtr : String -> LExpr
localPtr name = ptrExpr (Local name)

export
||| Create a global variable pointer expression.
globalPtr : String -> LExpr
globalPtr name = ptrExpr (Global name)

export
||| Create an LLVM module with configurable options.
mkModule : 
    {default Nothing dataLayout : Maybe String} ->
    {default Nothing target : Maybe String} ->
    {default [] text : List LClause} ->
    {default Nothing tags : Maybe (List LTag)} ->
    LModule
mkModule {dataLayout} {target} {text} {tags} = MkLModule dataLayout target text tags

export
||| Create a simple LLVM module with text clauses.
simpleModule : List LClause -> LModule
simpleModule clauses = mkModule {text = clauses}

export
||| Create a labeled statement.
label : String -> LStatement
label name = Labelled name

export
||| Create a targeted statement (assignment).
assign : Name -> LOperation -> LStatement
assign target op = Targeted target op

export
||| Create a discarded statement (no assignment).
discard : LOperation -> LStatement
discard op = Discarded op

export
||| Create a return statement.
ret : LType -> LExpr -> LStatement
ret ty expr = Discarded (TerminatorOp (Ret ty expr))

export
||| Create a void return statement.
retVoid : LStatement
retVoid = Discarded (TerminatorOp RetVoid)

export
||| Create a conditional branch statement.
condBr : LExpr -> LExpr -> LExpr -> LStatement
condBr cond trueLabel falseLabel = Discarded (TerminatorOp (CondBr cond trueLabel falseLabel))

export
||| Create an unconditional branch statement.
br : LExpr -> LStatement
br target = Discarded (TerminatorOp (JumpBr target))

export
||| Create a switch statement with default case and branches.
switch :
    (ty : LType) ->
    (value : LExpr) ->
    (defaultLabel : Name) ->
    (cases : List CaseBranch) ->
    LStatement
switch ty value defaultLabel cases = Discarded (TerminatorOp (Switch ty value defaultLabel cases))

export
||| Create an indirect branch through computed address.
indirectBr :
    (address : LExpr) ->
    (possibleDests : List LExpr) ->
    LStatement
indirectBr address dests = Discarded (TerminatorOp (IndirectBr address dests))

export
||| Create an invoke instruction (function call with exception handling).
invoke :
    (call : InvokeCall) ->
    LStatement
invoke call = Discarded (TerminatorOp (Invoke call))

export
||| Create an unreachable instruction.
unreachable : LStatement
unreachable = Discarded (TerminatorOp Unreachable)

export
||| Create an integer comparison operation.
icmp :
    (pred : Comparison) ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
icmp pred ty lhs rhs = MiscOp (FnCallOp (MkFnCall NoTail [] Nothing [] Nothing ty (LConstE (LPtr (IntrinsicN "icmp"))) [MkWithType ty lhs, MkWithType ty rhs] []))

export
||| Create a floating point comparison operation.
fcmp :
    (ordered : Bool) ->
    {default [] fastMath : FastMath} ->
    (pred : Comparison) ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
fcmp True {fastMath} pred ty lhs rhs = MiscOp (FnCallOp (MkFnCall NoTail fastMath Nothing [] Nothing ty (LConstE (LPtr (IntrinsicN "fcmp.ord"))) [MkWithType ty lhs, MkWithType ty rhs] []))
fcmp False {fastMath} pred ty lhs rhs = MiscOp (FnCallOp (MkFnCall NoTail fastMath Nothing [] Nothing ty (LConstE (LPtr (IntrinsicN "fcmp.unord"))) [MkWithType ty lhs, MkWithType ty rhs] []))

export
||| Create division operation (unsigned).
udiv :
    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
udiv {exact = False} ty lhs rhs = BinaryOp UDiv ty lhs rhs
udiv {exact = True} ty lhs rhs = BinaryOp UDivExact ty lhs rhs

export
||| Create division operation (signed).
sdiv :
    {default False exact : Bool} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
sdiv {exact = False} ty lhs rhs = BinaryOp SDiv ty lhs rhs  
sdiv {exact = True} ty lhs rhs = BinaryOp SDivExact ty lhs rhs

export
||| Create floating point division operation.
fdiv :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
fdiv {fastMath} ty lhs rhs = BinaryOp (FDiv fastMath) ty lhs rhs

export
||| Create remainder operation (unsigned).
urem : LType -> LExpr -> LExpr -> LOperation
urem ty lhs rhs = BinaryOp URem ty lhs rhs

export
||| Create remainder operation (signed).
srem : LType -> LExpr -> LExpr -> LOperation  
srem ty lhs rhs = BinaryOp SRem ty lhs rhs

export
||| Create floating point remainder operation.
frem :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
frem {fastMath} ty lhs rhs = BinaryOp (FRem fastMath) ty lhs rhs

export
||| Create shift left operation.
shl :
    {default Nothing wrap : Maybe Wrapping} ->
    (ty : LType) ->
    (value : LExpr) ->
    (amount : LExpr) ->
    LOperation
shl {wrap = Nothing} ty value amount = BinaryOp Shl ty value amount
shl {wrap = Just w} ty value amount = BinaryOp (ShlWrap w) ty value amount

export
||| Create logical shift right operation.
lshr :
    {default False exact : Bool} ->
    (ty : LType) ->
    (value : LExpr) ->
    (amount : LExpr) ->
    LOperation
lshr {exact = False} ty value amount = BinaryOp LShr ty value amount
lshr {exact = True} ty value amount = BinaryOp LShrExact ty value amount

export
||| Create arithmetic shift right operation.
ashr :
    {default False exact : Bool} ->
    (ty : LType) ->
    (value : LExpr) ->
    (amount : LExpr) ->
    LOperation
ashr {exact = False} ty value amount = BinaryOp AShr ty value amount
ashr {exact = True} ty value amount = BinaryOp AShrExact ty value amount

export
||| Create bitwise AND operation.
and : LType -> LExpr -> LExpr -> LOperation
and ty lhs rhs = BinaryOp And ty lhs rhs

export
||| Create bitwise OR operation.
or :
    {default False disjoint : Bool} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
or {disjoint = False} ty lhs rhs = BinaryOp Or ty lhs rhs
or {disjoint = True} ty lhs rhs = BinaryOp DisjointOr ty lhs rhs

export
||| Create bitwise XOR operation.
xor : LType -> LExpr -> LExpr -> LOperation
xor ty lhs rhs = BinaryOp Xor ty lhs rhs

export
||| Create an alloca (stack allocation) operation.
alloca :
    (ty : LType) ->
    {default Nothing count : Maybe (WithType Nat)} ->
    {default Nothing align : Maybe Nat} ->
    {default Nothing addrSpace : Maybe AddressSpace} ->
    LOperation
alloca ty {count} {align} {addrSpace} = MemoryOp (Alloc ty count align addrSpace)

export
||| Create a simple load operation.
load :
    {default False volatile : Bool} ->
    (ty : LType) ->
    (ptr : LExpr) ->
    {default Nothing align : Maybe Nat} ->
    LOperation
load {volatile} ty ptr {align} = MemoryOp (LoadRegular volatile ty ptr align False False False False Nothing Nothing Nothing False)

export
||| Create a simple store operation.
store :
    {default False volatile : Bool} ->
    (value : WithType LExpr) ->
    (ptr : LExpr) ->
    {default Nothing align : Maybe Nat} ->
    LOperation
store {volatile} value ptr {align} = MemoryOp (StoreRegular volatile value ptr align False False)

export
||| Create an atomic load operation.
loadAtomic :
    {default False volatile : Bool} ->
    (ty : LType) ->
    (ptr : LExpr) ->
    {default Nothing scope : Maybe String} ->
    {default Nothing ordering : Maybe AtomicOrder} ->
    {default Nothing align : Maybe Nat} ->
    LOperation
loadAtomic {volatile} ty ptr {scope} {ordering} {align} = 
    MemoryOp (LoadAtomic volatile ty ptr scope ordering align False False)

export
||| Create an atomic store operation.
storeAtomic :
    {default False volatile : Bool} ->
    (value : WithType LExpr) ->
    (ptr : LExpr) ->
    {default Nothing scope : Maybe String} ->
    {default Nothing ordering : Maybe AtomicOrder} ->
    {default Nothing align : Maybe Nat} ->
    LOperation
storeAtomic {volatile} value ptr {scope} {ordering} {align} = 
    MemoryOp (StoreAtomic volatile value ptr scope ordering align False)

export
||| Create an add operation.
add : LType -> LExpr -> LExpr -> LOperation
add ty lhs rhs = BinaryOp Add ty lhs rhs

export
||| Create a subtract operation.
sub : LType -> LExpr -> LExpr -> LOperation
sub ty lhs rhs = BinaryOp Sub ty lhs rhs

export
||| Create a multiply operation.
mul : LType -> LExpr -> LExpr -> LOperation
mul ty lhs rhs = BinaryOp Mul ty lhs rhs

export
||| Create a function call operation.
call : FnCall -> LOperation
call fnCall = MiscOp (FnCallOp fnCall)

export
||| Create a simple function call operation.
simpleCall : LType -> LExpr -> List (WithType LExpr) -> LOperation
simpleCall ty fn args = call (simpleFnCall ty fn args)

export
||| Create a truncate operation.
trunc :
    {default Nothing wrap : Maybe Wrapping} ->
    (from : WithType LExpr) ->
    (to : LType) ->
    LOperation
trunc {wrap = Nothing} from to = ConversionOp (Trunc NoSignedUnsigned) from to
trunc {wrap = Just w} from to = ConversionOp (Trunc w) from to

export
||| Create a zero extend operation.
zext : (from : WithType LExpr) -> (to : LType) -> LOperation
zext from to = ConversionOp ZExt from to

export
||| Create a sign extend operation.
sext : (from : WithType LExpr) -> (to : LType) -> LOperation
sext from to = ConversionOp SExt from to

export
||| Create a bitcast operation.
bitcast : (from : WithType LExpr) -> (to : LType) -> LOperation
bitcast from to = ConversionOp BitCast from to

export
||| Create a floating point truncate operation.
fptrunc :
    {default [] fastMath : FastMath} ->
    (from : WithType LExpr) ->
    (to : LType) ->
    LOperation
fptrunc {fastMath} from to = ConversionOp (FPTrunc fastMath) from to

export
||| Create a floating point extend operation.
fpext :
    {default [] fastMath : FastMath} ->
    (from : WithType LExpr) ->
    (to : LType) ->
    LOperation
fpext {fastMath} from to = ConversionOp (FPExt fastMath) from to

export
||| Create an insert element operation.
insertElement :
    (vector : WithType LExpr) ->
    (element : WithType LExpr) ->
    (index : WithType LExpr) ->
    LOperation
insertElement vector element index = VectorOp (InsertElement vector element index)

export
||| Create an extract element operation.
extractElement :
    (vector : WithType LExpr) ->
    (index : WithType LExpr) ->
    LOperation
extractElement vector index = VectorOp (ExtractElement vector index)

export
||| Create a shuffle vector operation.
shuffleVector :
    (vec1 : WithType LExpr) ->
    (vec2 : WithType LExpr) ->
    (mask : WithType LExpr) ->
    LOperation
shuffleVector vec1 vec2 mask = VectorOp (ShuffleVector vec1 vec2 mask)

export
||| Create an extract value operation.
extractValue :
    (aggregate : WithType LExpr) ->
    (index : Nat) ->
    LOperation
extractValue aggregate index = AggregateOp (ExtractValue aggregate index)

export
||| Create an insert value operation.
insertValue :
    (aggregate : WithType LExpr) ->
    (element : WithType LExpr) ->
    (index : Nat) ->
    LOperation
insertValue aggregate element index = AggregateOp (InsertValue aggregate element index)

export
||| Create a PHI node.
phi :
    (ty : LType) ->
    (incomingValues : List (LExpr, Label)) ->
    LOperation
phi ty incoming = MiscOp (Phi ty incoming)

export
||| Create a select operation.
select :
    {default [] fastMath : FastMath} ->
    (condition : WithType LExpr) ->
    (trueValue : WithType LExpr) ->
    (falseValue : WithType LExpr) ->
    LOperation
select {fastMath} condition trueValue falseValue = MiscOp (Select fastMath condition trueValue falseValue)

export
||| Create a freeze operation.
freeze : (value : WithType LExpr) -> LOperation
freeze value = MiscOp (Freeze value)

export
||| Create an invoke call with configurable options.
invokeCall :
    {default Nothing cc : Maybe CallingConvention} ->
    {default [] returnAttrs : List Attribute} ->
    {default Nothing addressSpace : Maybe AddressSpace} ->
    (tpe : LType) ->
    (fnval : LExpr) ->
    (args : List LExpr) ->
    (normal : Label) ->
    (unwind : Label) ->
    InvokeCall
invokeCall {cc} {returnAttrs} {addressSpace} tpe fnval args normal unwind =
    MkInvokeCall cc returnAttrs addressSpace tpe fnval args normal unwind

-- Example usage of the builder functions
export
exampleModule : LModule
exampleModule = 
  mkModule {
    dataLayout = Just "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128",
    target = Just "x86_64-unknown-linux-gnu",
    text = [
      -- Simple global variable
      GlobalDefC $ globalDef "myGlobal" (LInt 32) {init = Just (LInt 42)},
      
      -- Function definition with custom calling convention
      FunctionDefC $ functionDef "add" {callingConvention = Just C} (LInt 32) 
        [functionArg (LInt 32) {name = Just "a"}, functionArg (LInt 32) {name = Just "b"}]
        (MkFunctionBody [
          label "entry",
          assign (local "result") (add (LInt 32) (localPtr "a") (localPtr "b")),
          ret (LInt 32) (localPtr "result")
        ])
    ]
  }

-- Example of a simple function call
export
exampleCall : LOperation
exampleCall = simpleCall 
  (LFun (LInt 32) [LInt 32, LInt 32]) 
  (globalPtr "add") 
  [withType (LInt 32) (constExpr (LInt 5)), withType (LInt 32) (constExpr (LInt 10))]

export 
bytecode : {default Nothing mainMod : Maybe String} -> {default [] modules : List (String, LModule)} ->
    Bytecode
bytecode {mainMod} {modules} = MkBytecode mainMod modules

export 
foriegnDec : 
    (name : String) ->
    {default [] args : List FunctionArgSpec} ->
    {default LVoid resType : LType} ->
    LClause
foriegnDec name {args} {resType} = FunctionDecC $ functionDec name resType args

export 
symbolInfo : 
  {default Nothing lnk : Maybe Linkage} ->
  {default Nothing prm : Maybe Preemption} ->
  {default Nothing vis : Maybe Visibility} ->
  {default Nothing sto : Maybe DLLStorage} ->
  SymbolInfo 
  
symbolInfo {lnk} {prm} {vis} {sto} = MkSymbolInfo lnk prm vis sto

-- Additional missing builders that were identified in the audit:

-- 1. Missing wrapped arithmetic operations
export
||| Create addition with wrapping flags.
addWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
addWrap wrap ty lhs rhs = BinaryOp (AddWrap wrap) ty lhs rhs

export
||| Create subtraction with wrapping flags.
subWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
subWrap wrap ty lhs rhs = BinaryOp (SubWrap wrap) ty lhs rhs

export
||| Create multiplication with wrapping flags.
mulWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
mulWrap wrap ty lhs rhs = BinaryOp (MulWrap wrap) ty lhs rhs

-- 2. Missing enhanced shift operations
export
||| Create shift left with wrapping flags.
shlWrap :
    (wrap : Wrapping) ->
    (ty : LType) ->
    (value : LExpr) ->
    (amount : LExpr) ->
    LOperation
shlWrap wrap ty value amount = BinaryOp (ShlWrap wrap) ty value amount

-- 3. Missing enhanced comparison builders with specific predicates
export
||| Integer comparison: equal.
icmpEq : LType -> LExpr -> LExpr -> LOperation
icmpEq = icmp CEq

export
||| Integer comparison: not equal.
icmpNe : LType -> LExpr -> LExpr -> LOperation
icmpNe = icmp CNe

export
||| Integer comparison: unsigned less than.
icmpULt : LType -> LExpr -> LExpr -> LOperation
icmpULt = icmp CULt

export
||| Integer comparison: signed less than.
icmpSLt : LType -> LExpr -> LExpr -> LOperation
icmpSLt = icmp CSLt

export
||| Integer comparison: unsigned greater than.
icmpUGt : LType -> LExpr -> LExpr -> LOperation
icmpUGt = icmp CUGt

export
||| Integer comparison: signed greater than.
icmpSGt : LType -> LExpr -> LExpr -> LOperation
icmpSGt = icmp CSGt

export
||| Integer comparison: unsigned less than or equal.
icmpULe : LType -> LExpr -> LExpr -> LOperation
icmpULe = icmp CULe

export
||| Integer comparison: signed less than or equal.
icmpSLe : LType -> LExpr -> LExpr -> LOperation
icmpSLe = icmp CSLe

export
||| Integer comparison: unsigned greater than or equal.
icmpUGe : LType -> LExpr -> LExpr -> LOperation
icmpUGe = icmp CUGe

export
||| Integer comparison: signed greater than or equal.
icmpSGe : LType -> LExpr -> LExpr -> LOperation
icmpSGe = icmp CSGe

-- 4. Missing advanced terminator builders
export
||| Create a resume instruction for exception propagation.
resume : LType -> LExpr -> LStatement
resume ty value = Discarded (TerminatorOp (Resume ty value))

export
||| Create a catch return instruction.
catchRet : LExpr -> Label -> LStatement
catchRet value label = Discarded (TerminatorOp (CatchRet value label))

export
||| Create a cleanup return to caller.
cleanupRetCaller : LExpr -> LStatement
cleanupRetCaller value = Discarded (TerminatorOp (CleanupRetCaller value))

export
||| Create a cleanup return to specific label.
cleanupRet : LExpr -> Label -> LStatement  
cleanupRet value label = Discarded (TerminatorOp (CleanupRet value label))

export
||| Create a call branch instruction.
callBR : BrCall -> LStatement
callBR call = Discarded (TerminatorOp (CallBR call))

-- 5. Missing exception handling operation builders
export
||| Create a landing pad instruction.
landingPad : LType -> List CatchClause -> LOperation
landingPad ty clauses = ExceptOp (LandingPad ty clauses)

export
||| Create a landing pad with cleanup.
landingPadCleanup : LType -> List CatchClause -> LOperation
landingPadCleanup ty clauses = ExceptOp (LandingPadCleanup ty clauses)

export
||| Create a catch pad instruction.
catchPad : Name -> LConst -> LOperation
catchPad name value = ExceptOp (CatchPad name value)

export
||| Create a cleanup pad instruction.
cleanupPad : Name -> LConst -> LOperation
cleanupPad name value = ExceptOp (CleanupPad name value)

export
||| Create a catch clause.
catching : LType -> LConst -> CatchClause
catching ty value = Catching ty value

export
||| Create a filter clause.
filtering : LType -> LConst -> CatchClause
filtering ty value = Filtering ty value

export
||| Create a catch switch instruction.
catchSwitch : Name -> Maybe Label -> List Label -> Maybe Label -> LStatement
catchSwitch name parent handlers unwind = 
    Discarded (TerminatorOp (CatchSwitchOp (MkCatchSwitch name parent handlers unwind)))

-- 6. Missing constant builders for new types
export
||| Create a poison constant.
mkPoison : LConst
mkPoison = LPoison

export
||| Create a zero constant.
mkZero : LConst
mkZero = LZero

export
||| Create a token constant.
mkToken : LConst
mkToken = LToken

export
||| Create a metadata constant.
mkMetadata : Metadata -> LConst
mkMetadata md = LMetadata md

export
||| Create a metadata tuple.
metadataTuple : List Metadata -> Metadata
metadataTuple elems = MetadataTuple elems

-- 7. Missing helper builders for expressions and names
export
||| Create a variable expression from a name.
varExpr : Name -> LExpr
varExpr name = LVar name

export
||| Create a variable expression from a local name.
localVar : String -> LExpr
localVar name = LVar (Local name)

export
||| Create a variable expression from a global name.
globalVar : String -> LExpr
globalVar name = LVar (Global name)

-- 8. Missing call branch instruction builder
export
||| Create a call branch instruction call specification.
brCall :
    {default Nothing cc : Maybe CallingConvention} ->
    {default [] returnAttrs : List Attribute} ->
    {default Nothing addressSpace : Maybe AddressSpace} ->
    (tpe : LType) ->
    (fnval : LExpr) ->
    (args : List LExpr) ->
    (fallthrough : Label) ->
    (indirect : List Label) ->
    BrCall
brCall {cc} {returnAttrs} {addressSpace} tpe fnval args fallthrough indirect =
    MkBrCall cc returnAttrs addressSpace tpe fnval args fallthrough indirect

export
||| Create a metadata string.
metadataString : String -> Metadata
metadataString str = MetadataString str

export
||| Create a metadata value.
metadataValue : WithType LConst -> Metadata
metadataValue value = MetadataValue value

export
||| Create custom metadata.
metadataCustom : String -> Metadata
metadataCustom custom = MetadataCustom custom

-- 14. Missing composite type builders  
export
||| Create a vector type.
vectorType : Int -> LType -> LType
vectorType count elemTy = LVector count elemTy

export
||| Create a scalable vector type.
scalableVectorType : Int -> LType -> LType
scalableVectorType count elemTy = LVectorScale count elemTy

export
||| Create an array type.
arrayType : Int -> LType -> LType
arrayType count elemTy = LArray count elemTy

export
||| Create a struct type.
structType : List LType -> LType
structType fields = LStruct fields

export
||| Create a packed struct type.
packedStructType : List LType -> LType
packedStructType fields = LPackedStruct fields

export
||| Create a function type.
functionType : LType -> List LType -> LType
functionType retTy argTys = LFun retTy argTys

export
||| Create a variadic function type.
varArgFunctionType : LType -> List LType -> LType -> LType
varArgFunctionType retTy argTys varTy = LFunVarArg retTy argTys varTy

export  
||| Create a fence operation.
fence :
    {default Nothing scope : Maybe String} ->
    {default Nothing ordering : Maybe AtomicOrder} ->
    LOperation
fence {scope} {ordering} = MemoryOp (Fence scope ordering)

export
||| Create a metadata node.
metadataNode : String -> Metadata
metadataNode name = MetadataNamed name

export
||| Create a floating point negation operation.
fneg : LType -> LExpr -> LOperation
fneg ty operand = UnaryOp FNeg ty operand

export
||| Create floating point addition with fast math flags.
fadd :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
fadd {fastMath} ty lhs rhs = BinaryOp (FAdd fastMath) ty lhs rhs

export
||| Create floating point subtraction with fast math flags.
fsub :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
fsub {fastMath} ty lhs rhs = BinaryOp (FSub fastMath) ty lhs rhs

export
||| Create floating point multiplication with fast math flags.
fmul :
    {default [] fastMath : FastMath} ->
    (ty : LType) ->
    (lhs : LExpr) ->
    (rhs : LExpr) ->
    LOperation
fmul {fastMath} ty lhs rhs = BinaryOp (FMul fastMath) ty lhs rhs
