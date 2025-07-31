module Data.LLVM.IR.Builders.Core


--import Data.LLVM.Class
import Data.LLVM.IR.Core       
--import Data.LLVM.Write
import Data.LLVM.IR.Ops
import Data.LLVM.IR.Program
import Data.LLVM.IR.Alias
import Data.List
import Data.Walk
import Data.LLVM.IR.Util


export
||| Create an empty function body with no statements.
|||
||| Returns an empty FunctionBody that can be used as a starting point
||| for building function definitions or as a neutral element for combining
||| function bodies.
emptyFunctionBody : FunctionBody
emptyFunctionBody = MkFunctionBody [] 

||| Semigroup instance for FunctionBody allowing concatenation of statement lists.
|||
||| Combines two function bodies by concatenating their statement lists,
||| enabling composition of function body parts.
Semigroup FunctionBody where 
    (<+>) (MkFunctionBody stmts1) (MkFunctionBody stmts2) = MkFunctionBody (stmts1 ++ stmts2)

||| Monoid instance for FunctionBody with empty function body as neutral element.
|||
||| Provides a neutral empty function body and associative combination operation
||| for building complex function bodies from parts.
Monoid FunctionBody where 
    neutral = emptyFunctionBody

export 
||| Create an empty LLVM module with no data layout, target, clauses, or tags.
|||
||| Returns a minimal LModule that can be used as a starting point for
||| building LLVM modules or as a default module.
emptyModule : LModule
emptyModule = MkLModule Nothing Nothing [] Nothing 

export 
||| Create empty symbol information with no linkage, preemption, visibility, or storage.
|||
||| Returns a default SymbolInfo with all optional fields set to Nothing,
||| suitable as a default for symbols that don't need special attributes.
emptySymbolInfo : SymbolInfo
emptySymbolInfo = MkSymbolInfo Nothing Nothing Nothing Nothing 
public export
||| Associate a type with an expression to create a typed value.
|||
||| Creates a WithType wrapper that pairs an LLVM type with an expression,
||| providing type information that LLVM requires for many operations.
||| This is essential for type safety in LLVM IR.
|||
||| @ ty The LLVM type of the expression
||| @ expr The expression to be typed
withType : 
    (ty : LType) ->
    (expr : LExpr) ->
    WithType LExpr
withType ty expr = MkWithType ty expr

export
||| Create a boolean constant.
|||
||| Creates an LLVM boolean constant from a Bool value.
||| In LLVM IR, booleans are represented as i1 (1-bit integers).
|||
||| @ b The boolean value (True or False)
mkBool : Bool -> LConst
mkBool b = LBool b

export
||| Create a string constant.
|||
||| Creates an LLVM string constant from a String value.
||| The string will be null-terminated in the generated LLVM IR.
|||
||| @ s The string value to create as a constant
mkString : String -> LConst
mkString s = LString s

export
||| Create a null pointer constant.
|||
||| Creates an LLVM null pointer constant, representing a pointer
||| with value zero (null). Used for pointer initialization and
||| null pointer checks.
mkNull : LConst
mkNull = LNull

export
||| Create an undefined constant.
|||
||| Creates an LLVM undefined value constant, representing an
||| unspecified value. Useful for optimization and when the
||| specific value doesn't matter.
mkUndefined : LConst
mkUndefined = LUndefined

export
||| Create a floating point constant.
|||
||| Creates an LLVM floating point constant from a string representation.
||| The string should contain a valid floating point literal that LLVM
||| can parse.
|||
||| @ f The string representation of the floating point value
mkFloat : String -> LConst
mkFloat f = LFloat f

export
||| Create an array constant.
|||
||| Creates an LLVM array constant from a list of typed constant elements.
||| All elements must have compatible types for the array type.
|||
||| @ elems List of typed constant elements for the array
mkArray : List (WithType LConst) -> LConst
mkArray elems = LArray elems

export
||| Create a struct constant.
|||
||| Creates an LLVM struct constant from a list of typed constant fields.
||| The fields are ordered and their types must match the struct definition.
|||
||| @ fields List of typed constant fields for the struct
mkStruct : List (WithType LConst) -> LConst
mkStruct fields = LStruct fields

export
||| Create a vector constant.
|||
||| Creates an LLVM vector constant from a list of typed constant elements.
||| All elements must have the same type and the count must match the vector type.
|||
||| @ elems List of typed constant elements for the vector
mkVector : List (WithType LConst) -> LConst
mkVector elems = LVector elems


export
||| Create an expression from a constant.
|||
||| Converts an LLVM constant into an expression that can be used
||| in operations and instructions. This is necessary because LLVM
||| distinguishes between constants and expressions syntactically.
|||
||| @ c The constant to convert to an expression
constExpr : LConst -> LExpr
constExpr c = LConstE c


export
||| Create a local variable reference.
|||
||| Creates a Name representing a local variable or temporary.
||| Local variables are scoped to the current function and are
||| prefixed with '%' in LLVM IR.
|||
||| @ name The string name of the local variable
local : String -> Name
local name = Local name

public export 
prefix 10 $@

public export 
($@) : String -> LExpr
($@) name = LVar $ Local name

export
||| Create a global variable reference.
|||
||| Creates a Name representing a global variable or function.
||| Global variables are visible across the entire module and are
||| prefixed with '@' in LLVM IR.
|||
||| @ name The string name of the global variable
global : String -> Name
global name = Global name


export
||| Create a pointer expression from a name.
|||
||| Creates an expression representing a pointer to the named entity.
||| This is useful for taking addresses of variables and functions.
|||
||| @ name The name of the entity to create a pointer to
ptrExpr : Name -> LExpr
ptrExpr name = LConstE (LPtr name)

export
||| Create a local variable pointer expression.
|||
||| Creates a pointer expression to a local variable by name.
||| This is a convenience function that combines local name creation
||| with pointer expression generation.
|||
||| @ name The string name of the local variable to point to
localPtr : String -> LExpr
localPtr name = ptrExpr (Local name)

export
||| Create a global variable pointer expression.
|||
||| Creates a pointer expression to a global variable by name.
||| This is a convenience function that combines global name creation
||| with pointer expression generation.
|||
||| @ name The string name of the global variable to point to
globalPtr : String -> LExpr
globalPtr name = ptrExpr (Global name)

public export 
prefix 10 $*

public export 
||| Convenient operator to create a pointer expression from a name.
|||
||| Creates a pointer expression from a Name. This is a shorthand
||| for `ptrExpr name` that makes pointer operations more readable.
|||
||| @ name The name to create a pointer expression for
($*) : Name -> LExpr
($*) name = ptrExpr name


export
||| Create a labeled statement.
|||
||| Creates a label statement that can serve as a target for branches,
||| jumps, and other control flow instructions. Labels mark specific
||| points in the code that can be referenced.
|||
||| @ name The string name of the label
label : String -> LStatement
label name = Labelled name

public export 
infix 0 $<

public export 
||| Convenient operator to create a targeted statement (assignment).
|||
||| Creates a statement that assigns the result of an operation to a
||| target variable. This is a shorthand for `Operation target op` that
||| makes assignment operations more readable.
|||
||| @ target The target variable name to assign to
||| @ op The operation whose result to assign
($<) : Name -> LOperation -> LStatement
($<) target op = Operation target op

export
||| Create a targeted statement (assignment).
|||
||| Creates a statement that assigns the result of an operation to a
||| target variable. The target can be a local or global variable name.
|||
||| @ target The target variable name to assign to
||| @ op The operation whose result to assign
assign : Name -> LOperation -> LStatement
assign target op = Operation target op



export
||| Create a discarded statement (no assignment).
|||
||| Creates a statement that executes an operation but discards its result.
||| This is used for operations that have side effects but whose return
||| value is not needed.
|||
||| @ op The operation to execute and discard
discard : LOperation -> LStatement
discard op = Operation Trash op


export 
||| Create symbol information with linkage, preemption, visibility, and storage options.
|||
||| Creates a SymbolInfo record that specifies how a symbol should be linked,
||| whether it can be preempted, its visibility scope, and DLL storage class.
||| All parameters are optional with sensible defaults.
|||
||| @ lnk Optional linkage type (private, internal, external, etc.)
||| @ prm Optional preemption specification
||| @ vis Optional visibility (default, hidden, protected)
||| @ sto Optional DLL storage class (dllimport, dllexport)
symbolInfo : 
  {default Nothing lnk : Maybe Linkage} ->
  {default Nothing prm : Maybe Preemption} ->
  {default Nothing vis : Maybe Visibility} ->
  {default Nothing sto : Maybe DLLStorage} ->
  SymbolInfo 
  
symbolInfo {lnk} {prm} {vis} {sto} = MkSymbolInfo lnk prm vis sto


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
||| Create a metadata node.
metadataNode : String -> Metadata
metadataNode name = MetadataNamed name
