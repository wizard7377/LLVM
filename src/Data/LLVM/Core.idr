||| Core LLVM IR data types and utility functions.
|||
||| This module defines the fundamental data types for representing LLVM IR constructs,
||| including linkage types, calling conventions, visibility modifiers, and basic value types.
||| It also provides utility functions for name escaping and type manipulation.
module Data.LLVM.Core

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
  Tail           : CallingConvention
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
||| LLVM identifier types for different kinds of names.
|||
||| LLVM uses different prefixes to distinguish between different types
||| of identifiers in the IR. Each name type has a specific purpose and
||| namespace within LLVM.
public export
data Name : Type where 
  ||| Local variable or register name (prefixed with %)
  Local : String -> Name
  ||| Global variable or function name (prefixed with @)
  Global : String -> Name
  ||| Special compiler-generated name (prefixed with $)
  Special : String -> Name
  ||| Metadata node name (prefixed with !)
  MetadataN : String -> Name
  ||| Attribute group name (prefixed with #)
  AttributeN : String -> Name
  ||| Basic block label name (suffixed with :)
  LabelN : String -> Name
  ||| LLVM intrinsic function name (starts with "@llvm.")
  IntrinsicN : String -> Name
  ||| User-defined custom name type
  CustomN : String -> Name

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
  data LTypeF : Type where
    ||| IEEE 754 half precision (16-bit)
    Half      : LTypeF
    ||| Brain floating point (16-bit)
    Bfloat    : LTypeF
    ||| IEEE 754 single precision (32-bit)
    LFloat    : LTypeF
    ||| IEEE 754 double precision (64-bit)
    LDouble   : LTypeF
    ||| IEEE 754 quadruple precision (128-bit)
    FP128     : LTypeF
    ||| x86 extended precision (80-bit)
    X86_FP80  : LTypeF
    ||| PowerPC double-double (128-bit)
    PPC_FP128 : LTypeF
  ||| LLVM type representation.
  |||
  ||| Represents all possible LLVM IR types including primitives,
  ||| aggregates, functions, and target-specific types.
  public export
  -- TODO: Target types
  data LType : Type where
    ||| Generic pointer type (opaque pointer)
    LPtr : LType
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
    LFloating : LTypeF -> LType
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
record FunctionArgSpec where 
  constructor MkFunctionArgSpec
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
mutual 
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
    MetadataString : String -> Metadata
    MetadataValue : WithType LConst -> Metadata
    MetadataCustom : String -> Metadata
  public export
  data LConst : Type where 
    ||| Integer constant value
    LInt : Int -> LConst
    ||| Floating-point constant (as string to preserve precision)
    LFloat : String -> LConst
    ||| Boolean constant (true/false)
    LBool : Bool -> LConst 
    ||| Null pointer constant
    LNull : LConst
    ||| Token constant for state tracking
    LToken : LConst 
    ||| String literal constant
    LString : String -> LConst
    ||| Array constant with typed elements
    LArray : List (WithType LConst) -> LConst
    ||| Vector constant with typed elements
    LVector : List (WithType LConst) -> LConst
    ||| Structure constant with typed fields
    LStruct : List (WithType LConst) -> LConst
    ||| Undefined value (undefined behavior if used)
    LUndefined : LConst
    ||| Poison value (more undefined than undefined)
    LPoison : LConst
    ||| Zero initializer for any type
    LZero : LConst
    ||| Metadata constant
    LMetadata : Metadata -> LConst
    ||| Pointer to named global/function
    LPtr : Name -> LConst
    -- TODO: Basic block, dso-local, pointer auth, constant expression
    
||| LLVM expression namespace.
|||
||| Contains types for representing LLVM expressions and values.
||| Currently supports constant expressions with plans for more complex expressions.
namespace LExpr 
  ||| LLVM expression representation.
  |||
  ||| Represents values that can be used in LLVM IR instructions,
  ||| including constants and more complex expressions.
  public export
  data LExpr : Type where 
    ||| Constant expression wrapping a constant value
    LConstE : LConst -> LExpr
    LVar : Name -> LExpr 

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

||| Type alias for basic block labels.
|||
||| Labels are represented as expressions for flexibility in referencing
||| different types of basic block identifiers.
public export
Label : Type
Label = LExpr

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


