module Data.LLVM.Builders 

import Data.LLVM.Class
import Data.LLVM.Core       
import Data.LLVM.Write
import Data.LLVM.Ops
import Data.LLVM.Program

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
        ty2
        target 
        []

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
module_ : 
    {default Nothing dataLayout : Maybe String} ->
    {default Nothing target : Maybe String} ->
    {default [] text : List LClause} ->
    {default Nothing tags : Maybe (List LTag)} ->
    LModule
module_ {dataLayout} {target} {text} {tags} = MkLModule dataLayout target text tags

export
||| Create a simple LLVM module with text clauses.
simpleModule : List LClause -> LModule
simpleModule clauses = module_ {text = clauses}

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

-- Example usage of the builder functions
export
exampleModule : LModule
exampleModule = 
  module_ {
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