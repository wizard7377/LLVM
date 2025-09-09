module Data.LLVM.Builder.State 


import Data.LLVM.IR 
import Data.Has
import Data.SortedMap
import Control.Monad.Either
import Control.Monad.RWS
import Control.Monad.State
import public Data.Maybe
public export
Scope : Type -> Type 
Scope a = SortedMap a LType 
public export
WithScope : Type -> Type
WithScope a = (Scope Name, a)
-- At a later time do this
mutual
    ||| In general `ValueRef False` should have a block
    public export 
    record ValueRef {0 hasBlock : Bool} {0 hasType : Bool} (const : Bool) where
        constructor MkValueRef
        inBlock : Has BlockRef {p = hasBlock}
        ty : Has LType {p = hasType}
        value : LValue const


    public export 
    record BlockRef where 
        constructor MkBlockRef
        label : String
public export 
record FunctionRef where 
  constructor MkFunctionRef
  name : String
record GlobalRef where 
  constructor MkGlobalRef
  name : String
 
public export 
record TypeRef where 
  constructor MkTypeRef
  val : LType

public export
record BlockState where 
  constructor MkBlockState 
  ||| Statements, in reverse order
  statements : SnocList LStatement
  term : Maybe Terminator 
  scope : Scope Name
  name : String

public export 
record ModuleState where 
  constructor MkModuleState 
  val : LModule 
  later : SnocList LClause


public export 
record FunctionState where 
  constructor MkFunctionState 
  name : String
  ||| Seperate from basic block for reverse
  body : SnocList (String, BasicBlock)
  val : FunctionDef

public export 
record DeclarationState where 
  constructor MkDeclarationState 
  name : String
  val : FunctionDec

public export
data TopLevel : Type where 
  InFunction : FunctionState -> TopLevel
  InDeclare : DeclarationState -> TopLevel
  InNothing : TopLevel

  
export 
mkValueRef : {c : Bool} -> {default Nothing inBlock : Maybe BlockRef} -> {default Nothing ty : Maybe LType} -> LValue c -> ValueRef {hasBlock = (hasJust inBlock)} {hasType = (hasJust ty)} c
mkValueRef {inBlock} {ty} v = MkValueRef (maybeHas inBlock) (maybeHas ty) v

export 
mkBlockState : {default "" name : String} -> SnocList LStatement -> {default Nothing term : Maybe Terminator} -> {default empty scope : Scope Name} -> BlockState
mkBlockState {name} statements {term} {scope} = MkBlockState statements term scope name

  
public export 
label : BlockState -> BlockRef
label s = MkBlockRef s.name
