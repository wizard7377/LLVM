module Data.LLVM.Ops.Ops


--import Data.LLVM.Class
import Data.LLVM.IR.Core       
--import Data.LLVM.Write.Text.Encode
import Data.LLVM.IR.Core
import Data.LLVM.IR.Program
import Data.LLVM.IR.Alias
import Data.List
import Data.Walk
import Data.LLVM.IR.Util
import Data.LLVM.Ops.Core



public export
||| Make a global variable definition with configurable options.
|||
||| Creates a global variable definition with comprehensive configuration
||| options including symbol information, thread locality, address space,
||| and initialization settings.
|||
||| @ name The global variable name (identifier)
||| @ symbolInfo Symbol information including linkage, preemption, visibility, and storage
||| @ threadLocality Thread-local storage model if applicable
||| @ addressInfo Address information for the global variable
||| @ addressSpace Address space where the global variable resides
||| @ externallyInitialized Whether the variable is initialized externally
||| @ isConst Whether the global variable is constant (immutable)
||| @ ty The type of the global variable
||| @ init Optional initializer constant for the variable
||| @ tags Additional tags for the global variable
globalDef : 
    (name : String) ->
    {default emptySymbolInfo symbolInfo : SymbolInfo} ->
    {default Nothing threadLocality : Maybe ThreadLocality} ->
    {default Nothing addressInfo : Maybe AddressInfo} ->
    {default Nothing addressSpace : Maybe AddressSpace} ->
    {default Nothing externallyInitialized : Maybe Bool} ->
    {default False isConst : Bool} ->
    (ty : LType) ->
    {default Nothing init : Maybe (LValue ?)} ->
    {default neutral tags : Annotation} ->
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

public export
||| Create an alias for an existing global value.
|||
||| Creates an alias that provides an alternative name for an existing
||| global variable, function, or other global value. The alias has the
||| same type and points to the same target.
|||
||| @ name The alias name (identifier)
||| @ symbolInfo Symbol information including linkage, preemption, visibility, and storage
||| @ threadLocality Thread-local storage model if applicable
||| @ addressInfo Address information for the alias
||| @ ty The type of the aliased value
||| @ ty2 Alternative type specification (defaults to ty)
||| @ target The name of the target being aliased
||| @ tags Additional tags for the alias
alias : 
    (name : String) ->
    {default emptySymbolInfo symbolInfo : SymbolInfo} ->
    {default Nothing threadLocality : Maybe ThreadLocality} ->
    {default Nothing addressInfo : Maybe AddressInfo} ->
    (ty : LType) ->
    {default ty ty2 : LType} ->
    (target : String) ->
    {default neutral tags : Annotation} ->
    Alias
alias name {symbolInfo} {threadLocality} {addressInfo} ty {ty2} target {tags} =
    MkAlias
        name
        symbolInfo
        threadLocality
        addressInfo
        ty 
        target 
        neutral
        -- TODO: Remaining types


public export
||| Create an LLVM module with configurable options.
|||
||| Creates a complete LLVM module with optional data layout specification,
||| target triple, module contents, and metadata tags. This is the primary
||| way to construct LLVM modules programmatically.
|||
||| @ dataLayout Optional data layout string specifying target data layout
||| @ target Optional target triple string (e.g., "x86_64-unknown-linux-gnu")
||| @ text List of top-level clauses (functions, globals, etc.) in the module
||| @ tags Optional metadata tags for the module
mkModule : 
    {default Nothing dataLayout : Maybe String} ->
    {default Nothing target : Maybe String} ->
    (text : List LClause) ->
    {default neutral tags : Annotation} ->
    LModule
mkModule {dataLayout} {target} text {tags} = MkLModule dataLayout target text tags

public export
||| Create a simple LLVM module with text clauses.
|||
||| Creates a basic LLVM module with only the essential text clauses,
||| using default values for data layout, target triple, and tags.
||| This is the most common way to create modules.
|||
||| @ clauses List of top-level clauses (functions, globals, etc.) for the module
simpleModule : List LClause -> LModule
simpleModule clauses = mkModule clauses
