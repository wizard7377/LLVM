module Data.LLVM.Builders.Control 
--import Data.LLVM.Class
import Data.LLVM.IR.Core       
--import Data.LLVM.Write.Text.Encode
import Data.LLVM.IR.Core
import Data.LLVM.IR.Program
import Data.LLVM.IR.Alias
import Data.List
import Data.Walk
import Data.LLVM.IR.Util
import Data.LLVM.Builders.Core
import Data.LLVM.Builders.Ops
import Data.Table

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
    (args : List Argument) -> 
    {default Nothing addressInfo : Maybe AddressInfo} -> 
    {default Nothing addressSpace : Maybe AddressSpace} -> 
    {default [] fnAttributes : List Attribute} ->
    {default Nothing section : Maybe String} ->
    {default Nothing partition : Maybe String} ->
    {default Nothing comdat : Maybe Name} ->
    {default Nothing alignment : Maybe Int} ->
    {default Nothing gc : Maybe String} ->
    {default Nothing fprefix : Maybe (LValue True)} ->
    {default Nothing prologue : Maybe (LValue True)} ->
    {default Nothing personality : Maybe (LValue True)} ->
    {default [] metadata : List Metadata} ->
    (body : Table BasicBlock) ->
    {default neutral tags : Annotation} ->
    LClause
functionDef name {symbolInfo} {callingConvention} {returnAttrs} retType args {addressInfo} {addressSpace} {fnAttributes} {section} {partition} {comdat} {alignment} {gc} {fprefix} {prologue} {personality} {metadata} body {tags} =
    FunctionDefC $ MkFunctionDef
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
    (args : List Argument) ->
    {default Nothing addressInfo : Maybe AddressInfo} ->
    {default Nothing alignment : Maybe Int} ->
    {default Nothing gc : Maybe String} ->
    {default Nothing fprefix : Maybe (LValue True)} ->
    {default Nothing prologue : Maybe (LValue True)} ->
    {default neutral tags : Annotation} ->
    LClause
functionDec name {symbolInfo} {callingConvention} {returnAttrs} retType args {addressInfo} {alignment} {gc} {fprefix} {prologue} {tags} =
    FunctionDecC $ MkFunctionDec
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
intrinsicDec : 
    (name : IntrinsicName) ->
    (retType : LType) -> 
    (args : List Argument) ->
    LClause
intrinsicDec name retType args = IntrinsicDecC $ MkIntrinsicDec name retType args
export
||| Create a return statement.
|||
||| Creates a return statement that returns a typed value from the current
||| function. The return type must match the function's declared return type.
|||
||| @ ty The type of the value being returned
||| @ expr The expression representing the value to return
ret : LType -> (LValue _) -> Terminator
ret ty expr = (id (Ret ty $ toRuntime expr)) 

export
||| Create a void return statement.
|||
||| Creates a return statement for functions with void return type.
||| This terminates the function without returning a value.
retVoid : Terminator
retVoid = (id RetVoid) 

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
condBr : (LValue _) -> Label -> Label -> Terminator
condBr cond trueLabel falseLabel = (id (CondBr (toRuntime cond) trueLabel falseLabel)) 

export
||| Create an unconditional branch statement.
|||
||| Creates an unconditional jump to a target label. This is used
||| to implement simple control flow transfers like goto statements.
|||
||| @ target Expression representing the label to jump to
br : Label -> Terminator
br target = (id (JumpBr target)) 


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
    (address : (LValue _)) ->
    (possibleDests : List (LValue _)) ->
    Terminator
indirectBr address dests = (id (IndirectBr (toRuntime address) (toRuntime <$> dests))) 

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
    Terminator
invoke call = (id (Invoke call)) 

export
||| Create an unreachable instruction.
|||
||| Creates an unreachable instruction indicating that this point in the code
||| should never be reached during execution. This is used for optimization
||| and to indicate impossible code paths.
unreachable : Terminator
unreachable = (id Unreachable) 


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
    LExpr
alloca ty {count} {align} {addrSpace} = (Alloc ty count align addrSpace)

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
    (ptr : (LValue _)) ->
    {default Nothing align : Maybe Nat} ->
    LExpr
load {volatile} ty ptr {align} = (LoadRegular volatile ty (toRuntime ptr) align False False False False Nothing Nothing Nothing False)

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
    (value : WithType (LValue _)) ->
    (ptr : (LValue _)) ->
    {default Nothing align : Maybe Nat} ->
    LExpr
store {volatile} value ptr {align} = (StoreRegular volatile(toRuntime' value) (toRuntime ptr) align False False)

export
||| Create an atomic load operation.
loadAtomic :
    {default False volatile : Bool} ->
    (ty : LType) ->
    (ptr : (LValue _)) ->
    {default Nothing scope : Maybe String} ->
    {default Nothing ordering : Maybe AtomicOrder} ->
    {default Nothing align : Maybe Nat} ->
    LExpr
loadAtomic {volatile} ty ptr {scope} {ordering} {align} = 
    (LoadAtomic volatile ty (toRuntime ptr) scope ordering align False False)

export
||| Create an atomic store operation.
storeAtomic :
    {default False volatile : Bool} ->
    (value : WithType (LValue _)) ->
    (ptr : (LValue _)) ->
    {default Nothing scope : Maybe String} ->
    {default Nothing ordering : Maybe AtomicOrder} ->
    {default Nothing align : Maybe Nat} ->
    LExpr
storeAtomic {volatile} value ptr {scope} {ordering} {align} = 
    (StoreAtomic volatile(toRuntime' value) (toRuntime ptr) scope ordering align False)

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
    (value : (LValue _)) ->
    (defaultLabel : Label) ->
    (cases : List CaseBranch) ->
    Terminator
mkSwitch ty value defaultLabel cases = (id (Switch ty (toRuntime value) defaultLabel cases))

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
    (value : (LValue True)) ->
    (label : Label) -> 
    CaseBranch
caseBranch tpe value label = MkCaseBranch tpe ( value) label


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
    Argument
functionArg ty {attrs} {name} = MkArgument ty attrs name

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
    (fnval : (LValue _)) ->
    (args : List (WithType (LValue _))) ->
    {default [] fnAttrs : List Attribute} ->
    FnCall
fnCall {tail} {fastMath} {cc} {returnAttrs} {addressSpace} tpe fnval args {fnAttrs} =
    MkFnCall tail fastMath cc returnAttrs addressSpace tpe (toRuntime fnval) (toRuntime' <$> args) fnAttrs

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
        (fnval : (LValue _)) ->
        (args : List (WithType (LValue _))) ->
        FnCall
    simpleFnCall tpe fnval args = fnCall tpe fnval args


    export
    ||| Create a function call operation.
    call : FnCall -> LExpr
    call fnCall = (FnCallOp fnCall)

    export
    ||| Create a simple function call operation.
    simpleCall : {t0, t1 : Bool} -> LType -> (LValue t0) -> List (WithType (LValue t1)) -> LExpr
    simpleCall ty fn args = call (simpleFnCall ty fn args)
-- TODO: Finish making all of these not `_`



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
    {t0, t1, t2 : Bool} ->
    (vector : WithType (LValue t0)) ->
    (element : WithType (LValue t1)) ->
    (index : WithType (LValue t2)) ->
    LExpr
insertElement vector element index = (InsertElement (cast vector) (cast element) (cast index))

export
||| Create an extract element operation.
|||
||| Creates an operation that extracts a scalar element from a vector at the
||| specified index position. The index must be within the vector bounds.
|||
||| @ vector The typed source vector to extract from
||| @ index The typed index expression specifying the extraction position
extractElement :
    (vector : WithType (LValue _)) ->
    (index : WithType (LValue _)) ->
    LExpr
extractElement vector index = (ExtractElement (cast vector) (cast index))

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
    (vec1 : WithType (LValue _)) ->
    (vec2 : WithType (LValue _)) ->
    (mask : WithType (LValue _)) ->
    LExpr
shuffleVector vec1 vec2 mask = (ShuffleVector (cast vec1) (cast vec2) (cast mask))

export
||| Create an extract value operation.
|||
||| Creates an operation that extracts a value from an aggregate type (struct or array)
||| at the specified index. The index must be a compile-time constant and within bounds.
|||
||| @ aggregate The typed aggregate value to extract from
||| @ index The constant index specifying which field/element to extract
extractValue :
    (aggregate : WithType (LValue _)) ->
    (index : Nat) ->
    LExpr
extractValue aggregate index = (ExtractValue (cast aggregate) index)

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
    (aggregate : WithType (LValue _)) ->
    (element : WithType (LValue _)) ->
    (index : Nat) ->
    LExpr
insertValue aggregate element index = (InsertValue (cast aggregate) (cast element) index)

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
    (incomingValues : List ((LValue _), Label)) ->
    LExpr
phi ty incoming = (Phi ty $ each <$> incoming)
  where 
    each : (LValue _, Label) -> (LValue False, Label) 
    each (v, l) = (cast v, l)

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
    (condition : WithType (LValue _)) ->
    (trueValue : WithType (LValue _)) ->
    (falseValue : WithType (LValue _)) ->
    LExpr
select {fastMath} condition trueValue falseValue = (Select fastMath (toRuntime' condition) (toRuntime' trueValue) (toRuntime' falseValue))

export
||| Create a freeze operation.
|||
||| Creates a freeze operation that converts poison values to arbitrary
||| but fixed values. This is used to prevent undefined behavior propagation
||| in LLVM IR optimization.
|||
||| @ value The typed value to freeze
freeze : (value : WithType (LValue _)) -> LExpr
freeze value = (Freeze(toRuntime' value))

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
    (fnval : (LValue _)) ->
    (args : List (LValue _)) ->
    (normal : Label) ->
    (unwind : Label) ->
    InvokeCall
invokeCall {cc} {returnAttrs} {addressSpace} tpe fnval args normal unwind =
    MkInvokeCall cc returnAttrs addressSpace tpe (toRuntime fnval) (toRuntime <$> args) normal unwind

-- Example usage of the builder functions


-- 4. Missing advanced terminator builders
export
||| Create a resume instruction for exception propagation.
resume : LType -> (LValue _) -> Terminator
resume ty value =  (id (Resume ty (toRuntime value))) 

export
||| Create a catch return instruction.
catchRet : (LValue _) -> Label -> Terminator
catchRet value label =  (id (CatchRet (toRuntime value) label)) 

export
||| Create a cleanup return to caller.
cleanupRetCaller : (LValue _) -> Terminator
cleanupRetCaller value =  (id (CleanupRetCaller (toRuntime value))) 

export
||| Create a cleanup return to specific label.
cleanupRet : (LValue _) -> Label -> Terminator
cleanupRet value label =  (id (CleanupRet (toRuntime value) label)) 

export
||| Create a call branch instruction.
callBR : BrCall -> Terminator
callBR call =  (id (CallBR call)) 

-- 5. Missing exception handling operation builders
export
||| Create a landing pad instruction.
landingPad : LType -> List CatchClause -> LExpr
landingPad ty clauses = (LandingPad ty clauses)

export
||| Create a landing pad with cleanup.
landingPadCleanup : LType -> List CatchClause -> LExpr
landingPadCleanup ty clauses = (LandingPadCleanup ty clauses)

export
||| Create a catch pad instruction.
catchPad : Name -> (LValue _) -> LExpr
catchPad name value = (CatchPad name (toRuntime value))

export
||| Create a cleanup pad instruction.
cleanupPad : Name -> (LValue _) -> LExpr
cleanupPad name value = (CleanupPad name (toRuntime value))

export
||| Create a catch clause.
catching : LType -> (LValue _) -> CatchClause
catching ty value = Catching ty (toRuntime value)

export
||| Create a filter clause.
filtering : LType -> (LValue _) -> CatchClause
filtering ty value = Filtering ty (toRuntime value)

export
||| Create a catch switch instruction.
catchSwitch : Name -> Maybe Label -> List Label -> Maybe Label -> Terminator
catchSwitch name parent handlers unwind = 
    (id (CatchSwitchOp (MkCatchSwitch name parent handlers unwind))) 

-- 6. Missing constant builders for new types



-- 8. Missing call branch instruction builder
export
||| Create a call branch instruction call specification.
brCall :
    {default Nothing cc : Maybe CallingConvention} ->
    {default [] returnAttrs : List Attribute} ->
    {default Nothing addressSpace : Maybe AddressSpace} ->
    (tpe : LType) ->
    (fnval : (LValue _)) ->
    (args : List (LValue _)) ->
    (fallthrough : Label) ->
    (indirect : List Label) ->
    BrCall
brCall {cc} {returnAttrs} {addressSpace} tpe fnval args fallthrough indirect =
    MkBrCall cc returnAttrs addressSpace tpe (toRuntime fnval) (toRuntime <$> args) fallthrough indirect



export  
||| Create a fence operation.
fence :
    {default Nothing scope : Maybe String} ->
    {default Nothing ordering : Maybe AtomicOrder} ->
    LExpr
fence {scope} {ordering} = (Fence scope ordering)

