module Data.LLVM.Builder.Util 

import Data.LLVM.Builder.Types
import Data.LLVM.IR
import Data.LLVM.IR.Util
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Either
import Control.Monad.RWS
import Data.LLVM.Builder.Instances
import Data.List as Data.List
export 
getId : {m : Type -> Type} -> MonadBuilder m => m Int
getId = do
    st : BuilderState <- get 
    modify { uid $= (+ 1) }
    pure st.uid 
export 
genId : {m : Type -> Type} -> MonadBuilder m => m String 
genId = do 
    st : BuilderState <- get 
    let newId = "idris.LLVM.Internal.Builder.Gen" ++ show (st.uid) 
    modify { uid $= (+ 1) }
    pure newId

 
export 
userSpace : Show a => a -> String
userSpace s = "\"idris.LLVM.User." ++ show s ++ "\""
export
internalSpace : Show a => a -> String
internalSpace s = "\"idris.LLVM.Internal." ++ show s ++ "\""


export 
perhaps : MonadError _ m => m ? -> m ()
perhaps x = catchError (x >> pure ()) (\_ => pure ())

export 
trys : MonadError e m => m a -> m (Maybe a)
trys x = catchError (Just <$> x) (\_ => pure Nothing)

export 
maybeGenName : {m : Type -> Type} -> MonadBuilder m => Show a => Maybe a -> m String
maybeGenName Nothing = internalSpace <$> genId
maybeGenName (Just n) = userSpace <$> pure (show n)
  
export 
defGenName : {m : Type -> Type} -> MonadBuilder m => String -> m String
defGenName "" = internalSpace <$> genId
defGenName n = userSpace <$> pure (show n)

public export 
ChangeState : Type 
ChangeState = BuilderState -> BuilderState

public export 
infixl 7 <::
public export
(<::) : List a -> a -> List a
(<::) = Data.List.snoc -- TODO: Find a way to do this in better time (preferbly without `SnocList`)
