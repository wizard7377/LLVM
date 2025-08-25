||| A number of suggestions for performant LLVM IR of functional code
module Data.LLVM.Builders.Suggestions 
import Data.LLVM.IR
{- 
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
-}
export
suggestedCallingConvention : CallingConvention
suggestedCallingConvention = TailCC 

