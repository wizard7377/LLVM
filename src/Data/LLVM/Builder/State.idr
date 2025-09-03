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
  inBlock : Int 
  value : LValue False

public export 
record BlockRef where 
  constructor MkBlockRef
  label : Int
public export 
record FunctionRef where 
  constructor MkFunctionRef
  name : Int
record GlobalRef where 
  constructor MkGlobalRef
  name : Int
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
  name : Int
  val : FunctionDef

public export 
record DeclarationState where 
  constructor MkDeclarationState 
  name : Int
  val : FunctionDec
public export
data TopLevel : Type where 
  InFunction : FunctionState -> TopLevel
  InDeclare : DeclarationState -> TopLevel
  InNothing : TopLevel
