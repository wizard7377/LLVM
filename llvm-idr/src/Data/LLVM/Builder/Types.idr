module Data.LLVM.Builder.Types 

import Data.LLVM.IR 
import Data.SortedMap
import Control.Monad.Either
import Data.LLVM.Builder.State
import Control.Monad.RWS
import Control.Monad.State
import public Control.Monad.RWS.Interface
import public Control.Monad.Error.Interface
import public Control.Monad.State.Interface
import public Control.Monad.Reader.Interface
import public Control.Monad.Writer.Interface
public export 
data BuilderError : Type where 
  BasicError : String -> BuilderError
  NoBlock : BuilderError
  CantInfer : LValue False -> BuilderError
  OutsideFunction : String -> BuilderError
public export
record BuilderContext where 
    constructor MkBuilderContext


public export 
record BuilderState where 
  constructor MkBuilderState
  uid : Int 
  currentTopLevel : TopLevel
  currentBlock : Maybe BlockState 
  clauses : List LClause
 
public export
data MsgType : Type where
  BeginGroup : MsgType 
  EndGroup : MsgType
  Info : MsgType
  Warn : MsgType
  Debug : MsgType
public export
record BuilderLog where 
  constructor MkBuilderLog
  msgs : List (MsgType, String, Int )


export
Semigroup BuilderLog where 
  (<+>) (MkBuilderLog a) (MkBuilderLog b) = MkBuilderLog (a <+> b)
export 
Monoid BuilderLog where
  neutral = MkBuilderLog []

-----------------------
-- THE BUILDER MONAD --
-----------------------
export
data Builder : (m : Type -> Type) -> (a : Type) -> Type where 
  MkBuilder : EitherT (List BuilderError) (RWST BuilderContext BuilderLog BuilderState m) a -> Builder m a




defaultBuilderState : BuilderState
defaultBuilderState = MkBuilderState 0 (InNothing) Nothing []


export 
mkBuilder : {0 m : Type -> Type} -> EitherT (List BuilderError) (RWST BuilderContext BuilderLog BuilderState m) a -> Builder m a
mkBuilder = MkBuilder
export 
unBuilder : {m : Type -> Type} -> Builder m a -> EitherT (List BuilderError) (RWST BuilderContext BuilderLog BuilderState m) a
unBuilder (MkBuilder x) = x
export 
evalBuilder : {m : Type -> Type} -> Monad m => Builder m a ->  (context : BuilderContext) -> BuilderState -> m (Either (List BuilderError) a, BuilderState, BuilderLog)
evalBuilder (MkBuilder x) context state = do 
  (r0,r1,r2) <- runRWST context state (runEitherT x)
  pure (r0,r1,r2)

export 
runBuilder : {m : Type -> Type} -> Monad m => Builder m a ->  (context : BuilderContext) -> BuilderState -> m (Either (List BuilderError) a, BuilderLog)
runBuilder x context state = do
    (res, _, log) <- evalBuilder x context state
    ?h0
export 
execBuilder : {m : Type -> Type} -> Monad m => Builder m a ->  (context : BuilderContext) -> BuilderState -> m (Either (List BuilderError) a)
execBuilder x context state = do
    (res, _, _) <- evalBuilder x context state
    pure res
 

public export 
MonadBuilder : (m : Type -> Type) -> Type
MonadBuilder m = (Monad m , MonadState BuilderState m, MonadReader BuilderContext m , MonadError (List BuilderError) m , MonadWriter BuilderLog m) 
