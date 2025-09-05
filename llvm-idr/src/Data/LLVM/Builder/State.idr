module Data.LLVM.Builder.State 


import Data.LLVM.IR 
import Data.SortedMap
import Control.Monad.Either
import Control.Monad.RWS
import Control.Monad.State

public export
Scope : Type -> Type 
Scope a = SortedMap a LType 
public export
WithScope : Type -> Type
WithScope a = (Scope Name, a)

public export 
record ValueRef where 
  constructor MkValueRef
  inBlock : String 
  value : LValue False

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
export 
Cast ValueRef (LValue False) where 
  cast (MkValueRef _ v) = v

public export
record BlockState where 
  constructor MkBlockState 
  ||| Statements, in reverse order
  statements : List LStatement
  term : Maybe Terminator 
  scope : Scope Name
  name : String

public export 
record ModuleState where 
  constructor MkModuleState 
  val : LModule 


public export 
record FunctionState where 
  constructor MkFunctionState 
  name : String
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
