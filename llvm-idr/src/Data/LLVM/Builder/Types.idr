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
import public Data.SnocList
import Data.LLVM.Builder.Debug 
||| The set of errors that can occur during building
public export 
data BuilderError : Type where 
  BasicError : String -> BuilderError
  NoBlock : BuilderError
  CantInfer : LValue False -> BuilderError
  OutsideFunction : String -> BuilderError
  NoInstruction : String -> BuilderError
  AlreadyTerminated : String -> BuilderError

||| The context in which the builder is running
public export
record BuilderContext where 
    constructor MkBuilderContext
    debugLevel : Int

  
public export 
record DebugInfo where 
    constructor MkDebugInfo
    file : Maybe FileInfo
  
defaultDebugInfo : DebugInfo
defaultDebugInfo = MkDebugInfo Nothing
public export 
record BuilderState where 
  constructor MkBuilderState
  uid : Int 
  currentModule : Maybe ModuleState
  currentTopLevel : TopLevel
  currentBlock : Maybe BlockState 
  clauses : SnocList LClause
  debugInfo : DebugInfo
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
  msgs : SnocList (MsgType, String, Int )


export
Semigroup BuilderLog where 
  (<+>) (MkBuilderLog a) (MkBuilderLog b) = MkBuilderLog (a <+> b)
export 
Monoid BuilderLog where
  neutral = MkBuilderLog Lin

-----------------------
-- THE BUILDER MONAD --
-----------------------
||| The core builder monad transformer
||| Handles state, context, logging and errors
export
data Builder : (m : Type -> Type) -> (a : Type) -> Type where 
  MkBuilder : EitherT (List BuilderError) (RWST BuilderContext BuilderLog BuilderState m) a -> Builder m a




defaultBuilderState : BuilderState
defaultBuilderState = MkBuilderState 0 Nothing (InNothing) Nothing Lin defaultDebugInfo


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
