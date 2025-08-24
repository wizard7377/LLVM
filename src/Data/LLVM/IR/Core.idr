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
        value : (LValue True)        
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
        fnval : (LValue False)        
        ||| Function arguments
        args : List (LValue False)        
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
        fnval : (LValue False)        
        ||| Function arguments
        args : List (LValue False)        --fnAttrs : List ?
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
        fnval : (LValue False)        
        ||| Function arguments with their types
        args : List (WithType (LValue False))
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
        Ret : LType -> (LValue False)-> Terminator
        ||| Conditional branch (br i1 %cond, label %true, label %false)
        CondBr : (LValue False)-> Label -> Label -> Terminator
        ||| Unconditional branch (br label %target)
        JumpBr : Label -> Terminator
        ||| Switch statement with multiple cases
        Switch : LType -> (LValue False)-> Label -> List CaseBranch -> Terminator
        ||| Indirect branch through computed address
        IndirectBr : (LValue False)-> List (LValue False)-> Terminator
        ||| Invoke instruction (function call with exception handling)
        Invoke : InvokeCall -> Terminator
        ||| Call branch instruction (inline assembly with possible branches)
        CallBR : BrCall -> Terminator
        ||| Resume exception propagation
        Resume : LType -> (LValue False)-> Terminator
        ||| Unreachable code marker
        Unreachable : Terminator
        ||| Catch switch for exception handling
        CatchSwitchOp : CatchSwitch -> Terminator
        ||| Return from catch handler
        CatchRet : (LValue False)-> Label -> Terminator
        ||| Return from cleanup to caller
        CleanupRetCaller : (LValue False)-> Terminator
        ||| Return from cleanup to specific label
        CleanupRet : (LValue False)-> Label -> Terminator

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
        Catching : LType -> (LValue False)-> CatchClause
        ||| Filter clause for landing pad instructions
        Filtering : LType -> (LValue False)-> CatchClause



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
        Phi : LType -> List ((LValue False), Label) -> LExpr
        ||| Conditional select instruction
        Select : FastMath -> WithType (LValue False)-> WithType (LValue False)-> WithType (LValue False)-> LExpr
        ||| Freeze instruction (converts poison to undef)
        Freeze : WithType (LValue False)-> LExpr
        ||| Function call operation
        FnCallOp : FnCall -> LExpr
        -- [ ]: VaArg
        LandingPad : LType -> List CatchClause -> LExpr
        LandingPadCleanup : LType -> List CatchClause -> LExpr
        CatchPad : Name -> (LValue False)-> LExpr
        CleanupPad : Name -> (LValue False)-> LExpr
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
            (address : (LValue False)) -> 
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
            (address : (LValue False)) -> 
            (scope : Maybe String) -> 
            (ordering : Maybe AtomicOrder) ->
            (align : Maybe Nat) ->
            (nontemporal : Bool) -> 
            (invariantGroup : Bool) ->
            LExpr

        StoreRegular : 
            (volatile : Bool) ->
            (tpe : WithType (LValue False)) ->
            (address : (LValue False)) -> 
            (align : Maybe Nat) ->
            (nonTemporal : Bool) -> 
            (invariantGroup : Bool) ->
            LExpr
        StoreAtomic : 
            (volatile : Bool) ->
            (tpe : WithType (LValue False)) ->
            (address : (LValue False)) -> 
            (scope : Maybe String) -> 
            (ordering : Maybe AtomicOrder) ->
            (align : Maybe Nat) ->
            (invariantGroup : Bool) ->
            LExpr
        Fence : 
            (scope : Maybe String) ->
            (ordering : Maybe AtomicOrder) ->
            LExpr
        CmpXChg : (weak : Bool) -> (volatile : Bool) -> (LValue False)-> WithType (LValue False)-> WithType (LValue False)-> (syncscope : Maybe String) -> AtomicOrder -> AtomicOrder -> LExpr 
        
        -- TODO: Cmpxchg, atomicrmw, etc.
        ||| Unary operation opcodes.
        ||| Models LLVM IR unary instructions like:
        ||| ```llvm
        ||| %result = fneg float %x
        ||| ```

        ||| Floating point negation
        FNeg : LType -> (LValue False)-> LExpr

        ||| Binary operation opcodes for arithmetic and logical operations.
        ||| Models LLVM IR binary instructions like:
        ||| ```llvm
        ||| %result = add i32 %a, %b
        ||| %result = fadd float %x, %y
        ||| %result = and i1 %p, %q
        ||| %result = shl i32 %val, 2
        ||| ```
        |||All the simple binary opcodes

        Add : LType -> (LValue False)-> (LValue False)-> LExpr
        AddWrap : Wrapping -> LType -> (LValue False)-> (LValue False)-> LExpr
        FAdd : FastMath -> LType -> (LValue False)-> (LValue False)-> LExpr
        Sub : LType -> (LValue False)-> (LValue False)-> LExpr
        SubWrap : Wrapping -> LType -> (LValue False)-> (LValue False)-> LExpr
        FSub : FastMath -> LType -> (LValue False)-> (LValue False)-> LExpr
        Mul : LType -> (LValue False)-> (LValue False)-> LExpr
        MulWrap : Wrapping -> LType -> (LValue False)-> (LValue False)-> LExpr
        FMul : FastMath -> LType -> (LValue False)-> (LValue False)-> LExpr
        UDiv : LType -> (LValue False)-> (LValue False)-> LExpr
        UDivExact : LType -> (LValue False)-> (LValue False)-> LExpr
        SDiv : LType -> (LValue False)-> (LValue False)-> LExpr
        SDivExact : LType -> (LValue False)-> (LValue False)-> LExpr
        FDiv : FastMath -> LType -> (LValue False)-> (LValue False)-> LExpr
        URem : LType -> (LValue False)-> (LValue False)-> LExpr
        SRem : LType -> (LValue False)-> (LValue False)-> LExpr
        FRem : FastMath -> LType -> (LValue False)-> (LValue False)-> LExpr
        Shl : LType -> (LValue False)-> (LValue False)-> LExpr
        ShlWrap : Wrapping -> LType -> (LValue False)-> (LValue False)-> LExpr
        LShr : LType -> (LValue False)-> (LValue False)-> LExpr
        LShrExact : LType -> (LValue False)-> (LValue False)-> LExpr
        AShr : LType -> (LValue False)-> (LValue False)-> LExpr
        AShrExact : LType -> (LValue False)-> (LValue False)-> LExpr
        And : LType -> (LValue False)-> (LValue False)-> LExpr
        Or : LType -> (LValue False)-> (LValue False)-> LExpr
        DisjointOr : LType -> (LValue False)-> (LValue False)-> LExpr
        Xor : LType -> (LValue False)-> (LValue False)-> LExpr
        ||| Vector operation opcodes.
        ||| Models LLVM IR vector manipulation instructions like:
        ||| ```llvm
        ||| %result = insertelement <4 x i32> %vec, i32 %val, i32 0
        ||| %result = extractelement <4 x i32> %vec, i32 2
        ||| %result = shufflevector <4 x i32> %v1, <4 x i32> %v2, <4 x i32> <i32 0, i32 5, i32 2, i32 7>
        ||| ```

        ||| Insert element into vector at specified index
        InsertElement : WithType (LValue False)-> WithType (LValue False)-> WithType (LValue False)-> LExpr
        ||| Shuffle two vectors according to mask
        ShuffleVector : WithType (LValue False)-> WithType (LValue False)-> WithType (LValue False)-> LExpr
        ||| Extract element from vector at specified index
        ExtractElement : WithType (LValue False)-> WithType (LValue False)-> LExpr

        ||| Aggregate operation opcodes for structs and arrays.
        ||| Models LLVM IR aggregate manipulation instructions like:
        ||| ```llvm
        ||| %result = extractvalue {i32, float} %agg, 0
        ||| %result = insertvalue {i32, float} %agg, i32 42, 0
        ||| ```

        ||| Extract value from aggregate at specified index
        ExtractValue : WithType (LValue False)-> Nat -> LExpr
        ||| Insert value into aggregate at specified index
        InsertValue : WithType (LValue False)-> WithType (LValue False)-> Nat -> LExpr
        ||| Type conversion operation opcodes.
        ||| Models LLVM IR conversion instructions like:
        ||| ```llvm
        ||| %result = trunc i32 %val to i16
        ||| %result = zext i16 %val to i32
        ||| %result = bitcast i8* %ptr to i32*
        ||| %result = addrspacecast i8* %ptr to i8 addrspace(1)*
        ||| ```

        Trunc : Wrapping -> WithType (LValue False)-> LType -> LExpr
        ZExt : WithType (LValue False)-> LType -> LExpr
        SExt : WithType (LValue False)-> LType -> LExpr
        FPTrunc : FastMath -> WithType (LValue False)-> LType -> LExpr
        FPExt : FastMath -> WithType (LValue False)-> LType -> LExpr
        FPToUi : WithType (LValue False)-> LType -> LExpr
        FPToSi : WithType (LValue False)-> LType -> LExpr
        UiToFP : WithType (LValue False)-> LType -> LExpr
        SiToFP : WithType (LValue False)-> LType -> LExpr
        PtrToInt : WithType (LValue False)-> LType -> LExpr
        -- TODO: IntToPtr : LExpr
        BitCast : WithType (LValue False)-> LType -> LExpr
        AddrSpaceCast : AddressSpace -> WithType (LValue False)-> LType -> LExpr
        ||| Comparison operation opcodes.
        ||| Models LLVM IR comparison instructions like:
        ||| ```llvm
        ||| %result = icmp eq i32 %a, %b
        ||| %result = fcmp olt float %x, %y
        ||| %result = fcmp true float %a, %b    ; always true
        ||| ```

        ICmp : Comparison -> LType -> (LValue False)-> (LValue False)-> LExpr
        ICmpSign : Comparison -> LType -> (LValue False)-> (LValue False)-> LExpr
        FCmpOrd : FastMath -> Comparison -> LType -> (LValue False)-> (LValue False)-> LExpr
        FCmpUnOrd : FastMath -> Comparison -> LType -> (LValue False)-> (LValue False)-> LExpr
        FCmpFalse : LType -> (LValue False)-> (LValue False)-> LExpr
        FCmpTrue : LType -> (LValue False)-> (LValue False)-> LExpr
        ||| A special function not found within LLVM Assembly, although it does exist in the C API.
        ||| Essientally this specifies a way to "abstract" the ending of a function
        ||| It splits a basic block into two, end the first with the terminator of the function given the second block.
        ||| E.g, `WithCont Br` becomes `Br bb.unique: bb.unique`
        WithCont : (Label -> Terminator) -> LExpr

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
        MetadataValue : WithType (LValue True) -> Metadata
        MetadataCustom : String -> Metadata
        MetadataSpecial : String -> List (String, String) -> Metadata
        -- [ ]: Distinguish between constant and non-constant
    public export 
    data LConstExpr : Type where 
        LConstTrunc : (LValue True) -> LType -> LConstExpr
        LConstPtrToInt : (LValue True) -> LType -> LConstExpr
        LConstPtrToAddr : (LValue True) -> LType -> LConstExpr
        LConstIntToPtr : (LValue True) -> LType -> LConstExpr
        LConstBitcast : (LValue True) -> LType -> LConstExpr
        LConstAddrSpaceCast :  (LValue True) -> LType -> LConstExpr
        -- [ ]: Get element pointer
        LConstExtractElement : (LValue True) -> (LValue True) -> LConstExpr
        LConstInsertElement : (LValue True) -> (LValue True) -> LConstExpr
        LConstShuffleVector : (LValue True) -> (LValue True) -> LConstExpr
        LConstAdd : (LValue True) -> (LValue True) -> LConstExpr
        LConstSub : (LValue True) -> (LValue True) -> LConstExpr
        LConstXor : (LValue True) -> (LValue True) -> LConstExpr
    
    public export
    data LValue : (isConst : Bool) -> Type where 
        ||| Integer constant value
        LInt : Int -> (LValue True)
        ||| Floating-point constant (as string to preserve precision)
        LFloat : String -> (LValue True)
        ||| Boolean constant (true/false)
        LBool : Bool -> (LValue True) 
        ||| Null pointer constant
        LNull : (LValue True)
        ||| Token constant for state tracking
        LToken : (LValue True) 
        ||| String literal constant
        LString : String -> (LValue True)
        ||| Array constant with typed elements
        LArray : List (WithType (LValue True)) -> (LValue True)
        ||| Vector constant with typed elements
        LVector : List (WithType (LValue True)) -> (LValue True)
        ||| Structure constant with typed fields
        LStruct : List (WithType (LValue True)) -> (LValue True)
        ||| Undefined value (undefined behavior if used)
        LUndefined : (LValue True)
        ||| Poison value (more undefined than undefined)
        LPoison : (LValue True)
        ||| Zero initializer for any type
        LZero : (LValue True)
        ||| Metadata constant
        LMetadata : Metadata -> (LValue True)
        ||| Pointer to named global/function
        LPtr : Name -> (LValue True)
        LDsoLocalEquivalent : String -> (LValue True)
        LNoCFI : String -> (LValue True)
        LConstE : LConstExpr -> (LValue True)
        LVar : Name -> LValue False
        LComplex : LExpr -> LValue False
        LConst : LValue True -> LValue False
        -- [ ]: PtrAuth 
    public export 
    toRuntime : {0 isConst : Bool} -> (val : LValue isConst) -> LValue False
    toRuntime (LInt x) = LConst $ LInt x
    toRuntime (LFloat x) = LConst $ LFloat x
    toRuntime (LBool x) = LConst $ LBool x
    toRuntime LNull = LConst $ LNull
    toRuntime LToken = LConst $ LToken
    toRuntime (LString x) = LConst $ LString x
    toRuntime (LArray x) = LConst $ LArray x
    toRuntime (LVector x) = LConst $ LVector x
    toRuntime (LStruct x) = LConst $ LStruct x
    toRuntime LUndefined = LConst $ LUndefined
    toRuntime LPoison = LConst $ LPoison
    toRuntime LZero = LConst $ LZero
    toRuntime (LMetadata x) = LConst $ LMetadata x
    toRuntime (LPtr x) = LConst $ LPtr x
    toRuntime (LDsoLocalEquivalent x) = LConst $ LDsoLocalEquivalent x
    toRuntime (LNoCFI x) = LConst $ LNoCFI x
    toRuntime (LConstE x) = LConst $ LConstE x
    toRuntime (LVar x) = LVar x
    toRuntime (LComplex x) = LComplex x
    toRuntime (LConst x) = LConst x
    public export
    fromConst : {isConst : Bool} -> LValue True -> LValue isConst
    fromConst {isConst=True} x = x
    fromConst {isConst=False} x = LConst x
    public export 
    toRuntime' : {0 isConst : Bool} -> (val : WithType (LValue isConst)) -> WithType (LValue False)
    toRuntime' (MkWithType ty val) = MkWithType ty $ toRuntime val
    public export
    fromConst' : {isConst : Bool} -> WithType (LValue True) -> WithType (LValue isConst)
    fromConst' {isConst=True} x = x
    fromConst' {isConst=False} (MkWithType ty x) = MkWithType ty $ LConst x
    public export
    {0 isConst : Bool} -> Cast (LValue isConst) (LValue False) where 
      cast = toRuntime 
    public export
    {0 isConst : Bool} -> Cast (WithType (LValue isConst)) (WithType (LValue False)) where 
      cast = toRuntime'
    public export
    {isConst : Bool} -> Cast (LValue True) (LValue isConst) where 
      cast = fromConst
    public export
    {isConst : Bool} -> Cast (WithType (LValue True)) (WithType (LValue isConst)) where 
      cast = fromConst'
    %inline
    public export 
    ALValue : Type
    ALValue = {isConst : Bool} -> LValue isConst
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
