||| Core LLVM IR data types and utility functions.
|||
||| This module defines the fundamental data types for representing LLVM IR constructs,
||| including linkage types, calling conventions, visibility modifiers, and basic value types.
||| It also provides utility functions for name escaping and type manipulation.
module Data.LLVM.IR.Core


||| Escape special characters in identifiers to make them safe for LLVM IR.
|||
||| Converts non-alphanumeric characters to underscore-delimited names to ensure
||| valid LLVM identifiers. For example, '!' becomes "_EXCL_", ' ' becomes "_SPACE_".
||| Used internally for generating safe LLVM identifier names.
|||
||| @ c The character to escape
||| Returns the escaped string representation
unescapeC : Char -> String
unescapeC ' ' = "_SPACE_"
unescapeC '\n' = "_NEWLINE_"
unescapeC '\t' = "_TAB_"
unescapeC '\r' = "_CR_"
unescapeC '\f' = "_FF_"
unescapeC '\v' = "_VT_"
unescapeC '\\' = "_BACKSLASH_"
unescapeC '"' = "_QUOTE_"
unescapeC '\'' = "_SQUOTE_"
unescapeC '_' = "_UNDERSCORE_"
-- Punctuation and symbols
unescapeC '!' = "_EXCL_"
unescapeC '#' = "_HASH_"
unescapeC '$' = "_DOLLAR_"
unescapeC '%' = "_PERCENT_"
unescapeC '&' = "_AMP_"
unescapeC '(' = "_LPAREN_"
unescapeC ')' = "_RPAREN_"
unescapeC '*' = "_STAR_"
unescapeC '+' = "_PLUS_"
unescapeC ',' = "_COMMA_"
unescapeC '-' = "_MINUS_"
--unescapeC '.' = "_DOT_"
unescapeC '/' = "_SLASH_"
--unescapeC ':' = "_COLON_"
unescapeC ';' = "_SEMICOLON_"
unescapeC '<' = "_LT_"
unescapeC '=' = "_EQ_"
unescapeC '>' = "_GT_"
unescapeC '?' = "_QUESTION_"
unescapeC '@' = "_AT_"
unescapeC '[' = "_LBRACKET_"
unescapeC ']' = "_RBRACKET_"
unescapeC '^' = "_CARET_"
unescapeC '`' = "_BACKTICK_"
unescapeC '{' = "_LBRACE_"
unescapeC '|' = "_PIPE_"
unescapeC '}' = "_RBRACE_"
unescapeC '~' = "_TILDE_"
unescapeC c = pack [c]

||| Escape all special characters in a string.
|||
||| Applies `unescapeC` to every character in the input string and concatenates
||| the results. This ensures the entire string is safe for use as an LLVM identifier.
|||
||| @ s The string to escape
||| Returns the escaped string with all special characters replaced
public export 
unescape : String -> String
unescape s =  pack $ foldMap (unpack . unescapeC) (unpack s)


||| Linkage types that specify how symbols are linked and their visibility.
|||
||| LLVM linkage determines symbol resolution, optimization opportunities,
||| and visibility across module boundaries. Maps directly to LLVM IR linkage types.
|||
||| Examples:
||| - `Private`: Symbol not visible outside the module
||| - `Internal`: Symbol has internal linkage (like C static)
||| - `External`: Symbol can be linked from other modules
||| - `Weak`: Symbol can be replaced by another definition
public export
data Linkage : Type where
  ||| Symbol not visible outside this module
  Private      : Linkage
  ||| Symbol has internal linkage (like C static)
  Internal     : Linkage
  ||| Available externally for inspection but not linking
  Available    : Linkage
  ||| Link once semantics - can be replaced
  LinkOnce     : Linkage
  ||| Weak linkage - can be replaced by non-weak definitions
  Weak         : Linkage
  ||| Common linkage (for uninitialized globals)
  Common       : Linkage
  ||| Appending linkage (for arrays like llvm.global_ctors)
  Appending    : Linkage
  ||| External weak linkage
  ExternWeak   : Linkage
  ||| Link once ODR (One Definition Rule) semantics
  LinkOnceODR  : Linkage
  ||| Weak ODR linkage
  WeakODR      : Linkage
  ||| External linkage (default)
  External     : Linkage

||| Calling conventions that specify parameter passing and register usage.
|||
||| LLVM calling conventions determine how function parameters are passed,
||| which registers are used, and other ABI details. Different conventions
||| provide different performance characteristics and compatibility.
|||
||| Examples:
||| - `C`: Standard C calling convention
||| - `Fast`: Optimized for performance
||| - `Cold`: Optimized for infrequently called functions
public export
data CallingConvention : Type where
  ||| Standard C calling convention (default)
  C              : CallingConvention
  ||| Fast calling convention for performance
  Fast           : CallingConvention
  ||| Cold calling convention for rarely-called functions
  Cold           : CallingConvention
  ||| Glasgow Haskell Compiler calling convention
  GHC            : CallingConvention
  ||| High Performance Erlang calling convention
  CC11           : CallingConvention
  ||| Any register calling convention
  AnyReg         : CallingConvention
  ||| Preserve most registers calling convention
  PreserveMost   : CallingConvention
  ||| Preserve all registers calling convention
  PreserveAll    : CallingConvention
  ||| Preserve no registers calling convention
  PreserveNone   : CallingConvention
  ||| C++ fast thread-local calling convention
  CxxFastTL      : CallingConvention
  ||| Tail calling convention
  TailCC           : CallingConvention
  ||| Swift calling convention
  Swift          : CallingConvention
  ||| Swift tail calling convention
  SwiftTail      : CallingConvention
  ||| Control Flow Guard check calling convention
  CFGuardCheck   : CallingConvention
  ||| Custom calling convention with numeric ID
  CustomCC       : Int -> CallingConvention

||| Symbol visibility modifiers for controlling external visibility.
|||
||| Visibility attributes control how symbols are exposed when linking
||| shared libraries and executables. They correspond to ELF visibility
||| attributes and similar concepts on other platforms.
public export
data Visibility : Type where
  ||| Default visibility (symbol is externally visible)
  Default   : Visibility
  ||| Hidden visibility (symbol not visible outside shared object)
  Hidden    : Visibility
  ||| Protected visibility (symbol visible but not preemptible)
  Protected : Visibility

||| DLL storage class specifiers for Windows targets.
|||
||| These attributes specify import/export behavior for symbols
||| when targeting Windows DLL files.
public export
data DLLStorage : Type where
  ||| Symbol is exported from this DLL
  DLLExport : DLLStorage
  ||| Symbol is imported from another DLL
  DLLImport : DLLStorage

||| Thread-local storage models for thread-local variables.
|||
||| These specify different implementation strategies for thread-local storage,
||| each with different performance and dynamic loading characteristics.
public export
data ThreadLocality : Type where
  ||| Local dynamic TLS model
  LocalDynamic : ThreadLocality
  ||| Initial exec TLS model
  InitialExec  : ThreadLocality
  ||| Local exec TLS model
  LocalExec    : ThreadLocality

||| Preemption specifiers for symbol resolution.
|||
||| Controls whether symbols can be preempted (replaced) by other
||| definitions at link or load time.
public export
data Preemption : Type where
  ||| Symbol can be preempted by other definitions
  Preemptible    : Preemption
  ||| Symbol cannot be preempted (dso_local)
  NonPreemptible : Preemption

||| Address significance information for symbols.
|||
||| Indicates whether the address of a symbol is significant for
||| program semantics, affecting optimization opportunities.
public export
data AddressInfo : Type where
  ||| Global symbol address is not significant (unnamed_addr)
  UnnamedGlobal : AddressInfo
  ||| Local symbol address is not significant (local_unnamed_addr)
  UnnamedLocal  : AddressInfo

||| Address space specifications for memory layout.
|||
||| LLVM supports multiple address spaces to model different types of memory
||| (e.g., global vs. local memory in GPU architectures).
public export
data AddressSpace : Type where
  ||| Named address space
  NamedSpace : String -> AddressSpace
  ||| Numbered address space  
  UnnamedSpace : Int -> AddressSpace

public export 
data Register : Type where
  ||| Register with a specific name
  NamedRegister : String -> Register
  ||| Register with a numeric ID
  UnnamedRegister : Int -> Register

export
Show Register where
  show (NamedRegister name) = "%" ++ name
  show (UnnamedRegister id) = "%" ++ show id
export 
Eq Register where
  (==) (NamedRegister n1) (NamedRegister n2) = n1 == n2
  (==) (UnnamedRegister id1) (UnnamedRegister id2) = id1 == id2
  (==) _ _ = False
export 
Ord Register where  
  compare (NamedRegister n1) (NamedRegister n2) = compare n1 n2
  compare (UnnamedRegister id1) (UnnamedRegister id2) = compare id1 id2
  compare (NamedRegister _) (UnnamedRegister _) = LT
  compare (UnnamedRegister _) (NamedRegister _) = GT
||| LLVM identifier types for different kinds of names.
|||
||| LLVM uses different prefixes to distinguish between different types
||| of identifiers in the IR. Each name type has a specific purpose and
||| namespace within LLVM.
public export
data Name : Type where 
  Temporary : Nat -> Name
  Local : String -> Name 
  Parameter : String -> Name
  ||| Unnamed parameter 
  Unnamed : Nat -> Name
  Global : String -> Name
-- TODO: Allow for non-string idx
public export 
data Destination : Type where
  ||| A destination for a branch instruction (basic block label)
  Assign : Register -> Destination
  Nothing : Destination
  

export
implementation Eq Name where
  (==) (Local n1) (Local n2) = n1 == n2
  (==) (Global n1) (Global n2) = n1 == n2
  (==) (Temporary id1) (Temporary id2) = id1 == id2
  (==) (Parameter p1) (Parameter p2) = p1 == p2
  (==) (Unnamed id1) (Unnamed id2) = id1 == id2
  (==) _ _ = False
export
implementation Ord Name where
  compare (Local x) (Local y) = compare x y
  compare (Global x) (Global y) = compare x y
  compare (Temporary x) (Temporary y) = compare x y
  compare (Parameter x) (Parameter y) = compare x y
  compare (Unnamed x) (Unnamed y) = compare x y
  compare (Local _) _ = GT 
  compare _ (Local _) = LT 
  compare (Global _) _ = GT 
  compare _ (Global _) = LT
  compare (Temporary _) _ = GT
  compare _ (Temporary _) = LT
  compare (Parameter _) _ = GT
  compare _ (Parameter _) = LT
  compare (Unnamed _) _ = GT
  compare _ (Unnamed _) = LT

export 
implementation Show Name where
  show (Local n) = "local " ++ unescape n
  show (Global n) = "global " ++ unescape n
  show (Temporary id) = "temp " ++ show id
  show (Parameter n) = "arg " ++ unescape n
  show (Unnamed id) = "arg at " ++ show id
||| LLVM type system representation.
|||
||| This namespace contains types for representing all LLVM IR types,
||| including primitive types, aggregate types, and function types.
namespace LType 
  ||| Floating-point type formats supported by LLVM.
  |||
  ||| These correspond to different floating-point representations
  ||| with varying precision and range characteristics.
  public export 
  data LFloatFormat : Type where
    ||| IEEE 754 half precision (16-bit)
    Half      : LFloatFormat
    ||| Brain floating point (16-bit)
    Bfloat    : LFloatFormat
    ||| IEEE 754 single precision (32-bit)
    LFloat    : LFloatFormat
    ||| IEEE 754 double precision (64-bit)
    LDouble   : LFloatFormat
    ||| IEEE 754 quadruple precision (128-bit)
    FP128     : LFloatFormat
    ||| x86 extended precision (80-bit)
    X86_FP80  : LFloatFormat
    ||| PowerPC double-double (128-bit)
    PPC_FP128 : LFloatFormat
  mutual
    public export 
    data LTargetArg : Type where 
      TargetType : LType -> LTargetArg
      TargetInt : Int -> LTargetArg 
    ||| LLVM type representation.
    |||
    ||| Represents all possible LLVM IR types including primitives,
    ||| aggregates, functions, and target-specific types.
    public export
    -- DONE: Target types
    data LType : Type where
      ||| Generic pointer type (opaque pointer)
      LPtr : LType
      ||| Named type
      LNamed : String -> LType 
      ||| Pointer type to something (depreacted)
      LPtrTo : LType -> LType
      ||| Pointer to type in address space (depractated)
      LPtrToAddr : AddressSpace -> LType -> LType
      ||| Pointer in specific address space
      LPtrAddr : AddressSpace -> LType
      ||| Void type (no value)
      LVoid : LType
      ||| Function type with fixed arguments
      LFun : LType -> List LType -> LType
      ||| Variadic function type with fixed and variable arguments
      LFunVarArg : LType -> List LType -> LType -> LType
      ||| Opaque type (structure not defined)
      LOpaque : LType 
      ||| Integer type with specified bit width
      LInt : Int -> LType
      ||| Floating-point type
      LFloating : LFloatFormat -> LType
      ||| x86 Advanced Matrix Extensions type
      LX86_AMX : LType
      ||| Fixed-size vector type
      LVector : Int -> LType -> LType
      ||| Scalable vector type (size determined at runtime)
      LVectorScale : Int -> LType -> LType
      ||| Label type for basic block references
      LLabel : LType
      ||| Token type for representing state
      LToken : LType
      ||| Array type with fixed size and element type
      LArray : Int -> LType -> LType
      ||| Structure type with named fields
      LStruct : List LType -> LType
      ||| Packed structure type (no padding)
      LPackedStruct : List LType -> LType
      ||| Metadata type
      LMetadata :  LType
      LTarget : String -> List LTargetArg -> LType 
    
||| Wrapper for values with explicit type information.
|||
||| Many LLVM IR constructs require both a value and its type.
||| This record provides a convenient way to pair them together.
|||
||| @ a The type of the value being wrapped
public export
record WithType a where
  constructor MkWithType
  ||| The LLVM type of this value
  tpe : LType
  ||| The actual value
  value : a


||| Function and parameter attributes in LLVM IR.
|||
||| Attributes provide additional information about function parameters,
||| return values, and function behavior to enable optimizations and
||| specify ABI details.
public export
data Attribute : Type where
  ||| Zero extend parameter/return value
  ZeroExt : Attribute
  ||| Sign extend parameter/return value
  SignExt : Attribute
  ||| No extension (target-specific behavior)
  NoExt : Attribute
  ||| Pass by value with specified type
  ByVal : LType -> Attribute
  ||| Pass by reference with specified type
  ByRef : LType -> Attribute
  ||| Preallocated memory with specified type
  Preallocated : LType -> Attribute
  ||| In-alloca parameter with specified type
  Inalloca : LType -> Attribute
  ||| Structure return with specified type
  SRet : LType -> Attribute
  ||| Alignment requirement in bytes
  Align : Nat -> Attribute
  ||| No aliasing with other pointers
  NoAlias : Attribute
  ||| Memory is never freed
  NoFree : Attribute
  ||| Nested function parameter
  Nest : Attribute
  ||| Return value of function
  Returned : Attribute
  ||| Pointer is never null
  NoNull : Attribute
  ||| Pointer dereferenceable for N bytes
  Dereferenceable : Nat -> Attribute
  ||| Pointer dereferenceable for N bytes or null
  DereferenceableOrNull : Nat -> Attribute
  ||| Swift self parameter
  SwiftSelf : Attribute
  ||| Swift async parameter
  SwiftAsync : Attribute
  ||| Swift error parameter
  SwiftError : Attribute
  ||| Immediate argument (must be constant)
  ImmArg : Attribute
  ||| Value is not undefined (not undef or poison)
  NoUndef : Attribute
  ||| Stack alignment requirement
  AlignStack : Nat -> Attribute
  ||| Allocation alignment parameter
  AllocAlign : Attribute
  ||| Allocation pointer result
  AllocPtr : Attribute
  ||| Function/parameter doesn't read memory
  ReadNone : Attribute
  ||| Function/parameter only reads memory
  ReadOnly : Attribute
  ||| Function/parameter only writes memory
  WriteOnly : Attribute
  ||| Memory pointed to is writable
  Writeable : Attribute
  ||| Value is dead on exception unwind
  DeadOnUnwind : Attribute
  ||| Value is dead on function return
  DeadOnReturn : Attribute
  --AllocFamily : String -> Attribute 
  --AllocKind : String -> Attribute
  OtherAttribute : String -> Attribute
  --TODO: Finish



||| Function argument specification with type and attributes.
|||
||| Represents a single function parameter including its type,
||| any associated attributes, and optional name.
public export
record Argument where 
  constructor MkArgument
  ||| The type of this parameter
  type : LType
  ||| Attributes applied to this parameter
  attrs : List Attribute
  ||| Optional parameter name for documentation
  name : Maybe String 

||| Inline assembly representation.
|||
||| Allows embedding target-specific assembly code directly in LLVM IR.
||| Currently supports only basic inline assembly without constraints.
public export 
data Assembly : Type where 
  ||| Basic inline assembly with assembly string
  BasicAsm : String -> Assembly
-- TODO: Captures, nofp, inits, range, func atrributes


||| Custom metadata tags for LLVM constructs.
|||
||| Tags provide a way to attach custom metadata to various LLVM IR elements
||| for tooling, debugging, or custom optimization passes.
public export
data LTag : Type where 
  ||| A custom named tag
  CTag : String -> LTag
||| Symbol information record for global symbols.
|||
||| Collects various symbol properties that affect linking, visibility,
||| and optimization behavior. Used for both global variables and functions.
public export
record SymbolInfo where 
  constructor MkSymbolInfo 
  ||| Symbol linkage type
  linkage : Maybe Linkage
  ||| Symbol preemption specification
  preemption : Maybe Preemption
  ||| Symbol visibility
  visibility : Maybe Visibility
  ||| DLL storage class (Windows-specific)
  dllStorage : Maybe DLLStorage


||| Fast math optimization flags for floating-point operations.
|||
||| These flags enable various floating-point optimizations by relaxing
||| strict IEEE 754 compliance in exchange for improved performance.
public export 
data FastMathFlag : Type where
  ||| Enable all fast math optimizations
  FFast : FastMathFlag
  ||| Assume no NaN values exist
  NoNaNs : FastMathFlag
  ||| Assume no infinite values exist
  NoInfs : FastMathFlag
  ||| Assume signed zeros can be ignored
  NoSignedZeros : FastMathFlag

||| Collection of fast math flags.
|||
||| A list of fast math flags that can be applied to floating-point
||| operations to enable various optimizations.
public export 
FastMath : Type 
FastMath = List FastMathFlag

mutual
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
        
    ||| Type alias for basic block labels.
    |||
    ||| Labels are represented as expressions for flexibility in referencing
    ||| different types of basic block identifiers.
    public export
    data Label : Type where 
        NamedLabel : String -> Label
        LiftedLabel : BasicBlock -> Label

    ||| LLVM metadata representation.
    |||
    ||| Metadata provides additional information that doesn't affect program
    ||| semantics but can be used by debuggers, profilers, and other tools.
    public export 
    data Metadata : Type where
        ||| A metadata tuple containing multiple metadata elements
        MetadataTuple : List Metadata -> Metadata
        ||| Named metadata
        MetadataNamed : String -> Metadata
        MetadataNode : Nat -> Metadata
        MetadataString : String -> Metadata
        MetadataValue : WithType LValue -> Metadata
        MetadataCustom : String -> Metadata
        MetadataSpecial : String -> List (String, String) -> Metadata
        -- [ ]: Distinguish between constant and non-constant
    public export 
    data LConstExpr : Type where 
        LConstTrunc : LValue -> LType -> LConstExpr
        LConstPtrToInt : LValue -> LType -> LConstExpr
        LConstPtrToAddr : LValue -> LType -> LConstExpr
        LConstIntToPtr : LValue -> LType -> LConstExpr
        LConstBitcast : LValue -> LType -> LConstExpr
        LConstAddrSpaceCast :  LValue -> LType -> LConstExpr
        -- [ ]: Get element pointer
        LConstExtractElement : LValue -> LValue -> LConstExpr
        LConstInsertElement : LValue -> LValue -> LConstExpr
        LConstShuffleVector : LValue -> LValue -> LConstExpr
        LConstAdd : LValue -> LValue -> LConstExpr
        LConstSub : LValue -> LValue -> LConstExpr
        LConstXor : LValue -> LValue -> LConstExpr
    public export
    data LValue : Type where 
        ||| Integer constant value
        LInt : Int -> LValue
        ||| Floating-point constant (as string to preserve precision)
        LFloat : String -> LValue
        ||| Boolean constant (true/false)
        LBool : Bool -> LValue 
        ||| Null pointer constant
        LNull : LValue
        ||| Token constant for state tracking
        LToken : LValue 
        ||| String literal constant
        LString : String -> LValue
        ||| Array constant with typed elements
        LArray : List (WithType LValue) -> LValue
        ||| Vector constant with typed elements
        LVector : List (WithType LValue) -> LValue
        ||| Structure constant with typed fields
        LStruct : List (WithType LValue) -> LValue
        ||| Undefined value (undefined behavior if used)
        LUndefined : LValue
        ||| Poison value (more undefined than undefined)
        LPoison : LValue
        ||| Zero initializer for any type
        LZero : LValue
        ||| Metadata constant
        LMetadata : Metadata -> LValue
        ||| Pointer to named global/function
        LPtr : Name -> LValue
        -- TODO: Basic block, dso-local, pointer auth, constant expression
        LVar : Name -> LValue
        LDsoLocalEquivalent : String -> LValue
        LNoCFI : String -> LValue
        LConstE : LConstExpr -> LValue
        LComplex : LExpr -> LValue
        -- [ ]: PtrAuth 



    public export 
    record Annotation where 
        constructor MkAnnotation 
        metadata : List (String, Metadata)



    public export 
    Semigroup Annotation where 
        (MkAnnotation a) <+> (MkAnnotation b) = MkAnnotation (a ++ b)

    public export 
    Monoid Annotation where 
        neutral = MkAnnotation []