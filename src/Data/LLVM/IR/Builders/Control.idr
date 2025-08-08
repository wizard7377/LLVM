module Data.LLVM.IR.Builders.Control 
--import Data.LLVM.Class
import Data.LLVM.IR.Core       
--import Data.LLVM.Write.Assembly
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Program
import Data.LLVM.IR.Alias
import Data.List
import Data.Walk
import Data.LLVM.IR.Util
import Data.LLVM.IR.Builders.Core
import Data.LLVM.IR.Builders.Ops

export
||| Create a function definition with comprehensive configuration options.
|||
||| Builds a complete function definition with all possible LLVM function attributes
||| and metadata. Most parameters have sensible defaults to reduce verbosity.
|||
||| @ name The function name (identifier)
||| @ symbolInfo Symbol information including linkage, preemption, visibility, and storage
||| @ callingConvention The calling convention (C, FastCC, etc.)
||| @ returnAttrs Attributes applied to the return value
||| @ retType The return type of the function
||| @ args List of function argument specifications with types and attributes
||| @ addressInfo Address information for the function
||| @ addressSpace Address space where the function resides
||| @ fnAttributes Function-level attributes (noinline, readonly, etc.)
||| @ section Optional section name for the function
||| @ partition Optional partition name for the function
||| @ comdat Optional COMDAT group name
||| @ alignment Optional function alignment requirement
||| @ gc Optional garbage collector specification
||| @ fprefix Optional function prefix constant
||| @ prologue Optional prologue constant
||| @ personality Optional personality function for exception handling
||| @ metadata List of metadata attached to the function
||| @ body The function body containing statements
||| @ tags Additional tags for the function
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
    {default Nothing fprefix : Maybe LExpr} ->
    {default Nothing prologue : Maybe LExpr} ->
    {default Nothing personality : Maybe LExpr} ->
    {default [] metadata : List Metadata} ->
    (body : List Block) ->
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
||| Create a function declaration (forward declaration without body).
|||
||| Creates a function declaration that specifies the function signature
||| without providing an implementation. Used for external functions
||| or forward declarations.
|||
||| @ name The function name (identifier)
||| @ symbolInfo Symbol information including linkage, preemption, visibility, and storage
||| @ callingConvention The calling convention (C, FastCC, etc.)
||| @ returnAttrs Attributes applied to the return value
||| @ retType The return type of the function
||| @ args List of function argument specifications with types and attributes
||| @ addressInfo Address information for the function
||| @ alignment Optional function alignment requirement
||| @ gc Optional garbage collector specification
||| @ fprefix Optional function prefix constant
||| @ prologue Optional prologue constant
||| @ tags Additional tags for the function
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
    {default Nothing fprefix : Maybe LExpr} ->
    {default Nothing prologue : Maybe LExpr} ->
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
||| Create a return statement.
|||
||| Creates a return statement that returns a typed value from the current
||| function. The return type must match the function's declared return type.
|||
||| @ ty The type of the value being returned
||| @ expr The expression representing the value to return
ret : LType -> LExpr -> LStatement
ret ty expr = Operation Discard (TerminatorOp (Ret ty expr))

export
||| Create a void return statement.
|||
||| Creates a return statement for functions with void return type.
||| This terminates the function without returning a value.
retVoid : LStatement
retVoid = Operation Discard (TerminatorOp RetVoid)

export
||| Create a conditional branch statement.
|||
||| Creates a conditional branch that jumps to one of two labels based
||| on a boolean condition. This is the primary way to implement if-then-else
||| control flow in LLVM IR.
|||
||| @ cond Boolean expression to test (must be i1 type)
||| @ trueLabel Expression representing the label to jump to if condition is true
||| @ falseLabel Expression representing the label to jump to if condition is false
condBr : LExpr -> LExpr -> LExpr -> LStatement
condBr cond trueLabel falseLabel = Operation Discard (TerminatorOp (CondBr cond trueLabel falseLabel))

export
||| Create an unconditional branch statement.
|||
||| Creates an unconditional jump to a target label. This is used
||| to implement simple control flow transfers like goto statements.
|||
||| @ target Expression representing the label to jump to
br : LExpr -> LStatement
br target = Operation Discard (TerminatorOp (JumpBr target))


export
||| Create an indirect branch through computed address.
|||
||| Creates an indirect branch instruction that jumps to an address computed
||| at runtime. The address expression must evaluate to a valid label address,
||| and possible destinations must be provided for analysis.
|||
||| @ address Expression that computes the target address at runtime
||| @ possibleDests List of possible destination labels for static analysis
indirectBr :
    (address : LExpr) ->
    (possibleDests : List LExpr) ->
    LStatement
indirectBr address dests = Operation Discard (TerminatorOp (IndirectBr address dests))

export
||| Create an invoke instruction (function call with exception handling).
|||
||| Creates an invoke instruction that calls a function with exception handling
||| support. If the function throws an exception, control transfers to the
||| unwind label; otherwise, it continues to the normal label.
|||
||| @ call The invoke call specification with function, arguments, and labels
invoke :
    (call : InvokeCall) ->
    LStatement
invoke call = Operation Discard (TerminatorOp (Invoke call))

export
||| Create an unreachable instruction.
|||
||| Creates an unreachable instruction indicating that this point in the code
||| should never be reached during execution. This is used for optimization
||| and to indicate impossible code paths.
unreachable : LStatement
unreachable = Operation Discard (TerminatorOp Unreachable)


export
||| Create an alloca (stack allocation) operation.
|||
||| Creates a stack allocation instruction that allocates memory on the
||| function's stack frame. The memory is automatically deallocated when
||| the function returns.
|||
||| @ ty The type of objects to allocate
||| @ count Optional number of objects to allocate (defaults to single object)
||| @ align Optional alignment requirement for the allocation
||| @ addrSpace Optional address space for the allocation
alloca :
    (ty : LType) ->
    {default Nothing count : Maybe (WithType Nat)} ->
    {default Nothing align : Maybe Nat} ->
    {default Nothing addrSpace : Maybe AddressSpace} ->
    LOperation
alloca ty {count} {align} {addrSpace} = MemoryOp (Alloc ty count align addrSpace)

export
||| Create a simple load operation.
|||
||| Creates a load instruction that reads a value from memory. The pointer
||| must point to a valid memory location containing a value of the specified type.
|||
||| @ volatile Whether this is a volatile load (prevents optimization)
||| @ ty The type of the value to load from memory
||| @ ptr Expression representing the pointer to load from
||| @ align Optional alignment requirement for the load
load :
    {default False volatile : Bool} ->
    (ty : LType) ->
    (ptr : LExpr) ->
    {default Nothing align : Maybe Nat} ->
    LOperation
load {volatile} ty ptr {align} = MemoryOp (LoadRegular volatile ty ptr align False False False False Nothing Nothing Nothing False)

export
||| Create a simple store operation.
|||
||| Creates a store instruction that writes a typed value to memory at the
||| specified pointer location. The pointer must be valid and properly aligned.
|||
||| @ volatile Whether this is a volatile store (prevents optimization)
||| @ value The typed value to store in memory
||| @ ptr Expression representing the pointer to store to
||| @ align Optional alignment requirement for the store
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

||| Create a switch statement with default case and branches.
|||
||| Creates a switch statement (internal helper version) that transfers control
||| based on the value of an expression. The expression is compared against
||| each case value, and control transfers to the corresponding label.
|||
||| @ ty The type of the switch expression and case values
||| @ value The expression to switch on
||| @ defaultLabel The label to jump to if no cases match
||| @ cases List of case branches with values and target labels
mkSwitch' :
    (ty : LType) ->
    (value : LExpr) ->
    (defaultLabel : Name) ->
    (cases : List CaseBranch) ->
    LStatement
mkSwitch' ty value defaultLabel cases = Operation Discard (TerminatorOp (Switch ty value defaultLabel cases))

export
||| Create a switch statement with default case and branches.
|||
||| Creates a switch statement that transfers control based on the value
||| of an expression. The expression is compared against each case value,
||| and control transfers to the corresponding label.
|||
||| @ ty The type of the switch expression and case values
||| @ value The expression to switch on
||| @ defaultLabel The label to jump to if no cases match
||| @ cases List of case branches with values and target labels
mkSwitch :
    (ty : LType) ->
    (value : LExpr) ->
    (defaultLabel : Name) ->
    (cases : List CaseBranch) ->
    LOperation
mkSwitch ty value defaultLabel cases = (TerminatorOp (Switch ty value defaultLabel cases))

export
||| Create a case branch for switch statements.
|||
||| Defines a single case in a switch statement, mapping a value of a specific
||| type to a target label. The value is compared against the switch expression,
||| and if they match, control transfers to the specified label.
|||
||| @ tpe The type of the case value (must match switch expression type)
||| @ value The constant value to match against
||| @ label The target label to jump to when this case matches
caseBranch : 
    (tpe : LType) ->
    (value : LExpr) ->
    (label : Label) -> 
    CaseBranch
caseBranch tpe value label = MkCaseBranch tpe value label


export
||| Create a function argument specification with optional attributes and name.
|||
||| Creates a function argument specification that includes type information,
||| optional parameter attributes, and an optional parameter name for
||| better readability and debugging.
|||
||| @ ty The type of the function parameter
||| @ attrs Optional attributes applied to this parameter (e.g., noalias, readonly)
||| @ name Optional name for the parameter (for readability)
functionArg : 
    (ty : LType) ->
    {default [] attrs : List Attribute} ->
    {default Nothing name : Maybe String} ->
    FunctionArgSpec
functionArg ty {attrs} {name} = MkFunctionArgSpec ty attrs name

export
||| Create a function call with configurable options.
|||
||| Creates a comprehensive function call specification with all possible
||| call-site attributes and options. Most parameters have sensible defaults
||| to simplify common use cases.
|||
||| @ tail Tail call optimization specification (NoTail by default)
||| @ fastMath Fast math optimization flags for floating point operations
||| @ cc Calling convention override for this specific call
||| @ returnAttrs Attributes applied to the return value
||| @ addressSpace Address space for the function call
||| @ tpe The return type of the function call
||| @ fnval Expression representing the function to call
||| @ args List of typed arguments to pass to the function
||| @ fnAttrs Additional function attributes for this call
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

mutual
    export
    ||| Create a simple function call with minimal arguments.
    |||
    ||| Creates a basic function call without advanced options, using default
    ||| values for tail call optimization, calling convention, and attributes.
    ||| This is the most common way to create function calls.
    |||
    ||| @ tpe The return type of the function call
    ||| @ fnval Expression representing the function to call
    ||| @ args List of typed arguments to pass to the function
    simpleFnCall : 
        (tpe : LType) ->
        (fnval : LExpr) ->
        (args : List (WithType LExpr)) ->
        FnCall
    simpleFnCall tpe fnval args = fnCall tpe fnval args


    export
    ||| Create a function call operation.
    call : FnCall -> LOperation
    call fnCall = MiscOp (FnCallOp fnCall)

    export
    ||| Create a simple function call operation.
    simpleCall : LType -> LExpr -> List (WithType LExpr) -> LOperation
    simpleCall ty fn args = call (simpleFnCall ty fn args)




export
||| Create an insert element operation.
|||
||| Creates an operation that inserts a scalar element into a vector at the
||| specified index position. The vector type and element type must be compatible.
|||
||| @ vector The typed source vector to insert into
||| @ element The typed scalar element to insert
||| @ index The typed index expression specifying the insertion position
insertElement :
    (vector : WithType LExpr) ->
    (element : WithType LExpr) ->
    (index : WithType LExpr) ->
    LOperation
insertElement vector element index = VectorOp (InsertElement vector element index)

export
||| Create an extract element operation.
|||
||| Creates an operation that extracts a scalar element from a vector at the
||| specified index position. The index must be within the vector bounds.
|||
||| @ vector The typed source vector to extract from
||| @ index The typed index expression specifying the extraction position
extractElement :
    (vector : WithType LExpr) ->
    (index : WithType LExpr) ->
    LOperation
extractElement vector index = VectorOp (ExtractElement vector index)

export
||| Create a shuffle vector operation.
|||
||| Creates an operation that shuffles elements from two input vectors according
||| to a mask vector. The mask specifies which elements from which input vectors
||| to include in the result.
|||
||| @ vec1 The first typed input vector
||| @ vec2 The second typed input vector
||| @ mask The typed mask vector specifying the shuffle pattern
shuffleVector :
    (vec1 : WithType LExpr) ->
    (vec2 : WithType LExpr) ->
    (mask : WithType LExpr) ->
    LOperation
shuffleVector vec1 vec2 mask = VectorOp (ShuffleVector vec1 vec2 mask)

export
||| Create an extract value operation.
|||
||| Creates an operation that extracts a value from an aggregate type (struct or array)
||| at the specified index. The index must be a compile-time constant and within bounds.
|||
||| @ aggregate The typed aggregate value to extract from
||| @ index The constant index specifying which field/element to extract
extractValue :
    (aggregate : WithType LExpr) ->
    (index : Nat) ->
    LOperation
extractValue aggregate index = AggregateOp (ExtractValue aggregate index)

export
||| Create an insert value operation.
|||
||| Creates an operation that inserts a value into an aggregate type (struct or array)
||| at the specified index, returning a new aggregate with the updated value.
|||
||| @ aggregate The typed aggregate value to insert into
||| @ element The typed value to insert
||| @ index The constant index specifying where to insert the value
insertValue :
    (aggregate : WithType LExpr) ->
    (element : WithType LExpr) ->
    (index : Nat) ->
    LOperation
insertValue aggregate element index = AggregateOp (InsertValue aggregate element index)

export
||| Create a PHI node.
|||
||| Creates a PHI node for SSA form that selects a value based on which
||| basic block was the predecessor. Essential for control flow merging
||| in SSA form where multiple execution paths converge.
|||
||| @ ty The type of all the incoming values (must be the same)
||| @ incomingValues List of (value, label) pairs for each predecessor block
phi :
    (ty : LType) ->
    (incomingValues : List (LExpr, Label)) ->
    LOperation
phi ty incoming = MiscOp (Phi ty incoming)

export
||| Create a select operation.
|||
||| Creates a conditional select operation that chooses between two values
||| based on a boolean condition. This is similar to the ternary operator
||| in many languages (condition ? trueValue : falseValue).
|||
||| @ fastMath Optional fast math flags for floating point operations
||| @ condition The typed boolean condition to test
||| @ trueValue The typed value to select if condition is true
||| @ falseValue The typed value to select if condition is false
select :
    {default [] fastMath : FastMath} ->
    (condition : WithType LExpr) ->
    (trueValue : WithType LExpr) ->
    (falseValue : WithType LExpr) ->
    LOperation
select {fastMath} condition trueValue falseValue = MiscOp (Select fastMath condition trueValue falseValue)

export
||| Create a freeze operation.
|||
||| Creates a freeze operation that converts poison values to arbitrary
||| but fixed values. This is used to prevent undefined behavior propagation
||| in LLVM IR optimization.
|||
||| @ value The typed value to freeze
freeze : (value : WithType LExpr) -> LOperation
freeze value = MiscOp (Freeze value)

export
||| Create an invoke call with configurable options.
|||
||| Creates an invoke call specification for exception-handling function calls.
||| The invoke instruction calls a function and provides both a normal return
||| label and an exception unwind label for proper exception handling.
|||
||| @ cc Optional calling convention override for this call
||| @ returnAttrs Attributes applied to the return value
||| @ addressSpace Optional address space for the call
||| @ tpe The return type of the function being invoked
||| @ fnval Expression representing the function to invoke
||| @ args List of arguments to pass to the function
||| @ normal Label to continue to on normal return
||| @ unwind Label to unwind to on exception
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


-- 4. Missing advanced terminator builders
export
||| Create a resume instruction for exception propagation.
resume : LType -> LExpr -> LStatement
resume ty value = Operation Discard (TerminatorOp (Resume ty value))

export
||| Create a catch return instruction.
catchRet : LExpr -> Label -> LStatement
catchRet value label = Operation Discard (TerminatorOp (CatchRet value label))

export
||| Create a cleanup return to caller.
cleanupRetCaller : LExpr -> LStatement
cleanupRetCaller value = Operation Discard (TerminatorOp (CleanupRetCaller value))

export
||| Create a cleanup return to specific label.
cleanupRet : LExpr -> Label -> LStatement  
cleanupRet value label = Operation Discard (TerminatorOp (CleanupRet value label))

export
||| Create a call branch instruction.
callBR : BrCall -> LStatement
callBR call = Operation Discard (TerminatorOp (CallBR call))

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
catchPad : Name -> LExpr -> LOperation
catchPad name value = ExceptOp (CatchPad name value)

export
||| Create a cleanup pad instruction.
cleanupPad : Name -> LExpr -> LOperation
cleanupPad name value = ExceptOp (CleanupPad name value)

export
||| Create a catch clause.
catching : LType -> LExpr -> CatchClause
catching ty value = Catching ty value

export
||| Create a filter clause.
filtering : LType -> LExpr -> CatchClause
filtering ty value = Filtering ty value

export
||| Create a catch switch instruction.
catchSwitch : Name -> Maybe Label -> List Label -> Maybe Label -> LStatement
catchSwitch name parent handlers unwind = 
    Operation Discard (TerminatorOp (CatchSwitchOp (MkCatchSwitch name parent handlers unwind)))

-- 6. Missing constant builders for new types



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
||| Create a fence operation.
fence :
    {default Nothing scope : Maybe String} ->
    {default Nothing ordering : Maybe AtomicOrder} ->
    LOperation
fence {scope} {ordering} = MemoryOp (Fence scope ordering)
