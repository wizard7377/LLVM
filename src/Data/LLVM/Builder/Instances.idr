module Data.LLVM.Builder.Instances 

import Data.LLVM.Builder.Types
import Control.Monad.Either
import Control.Monad.RWS
import Control.Monad.Trans
import Control.Monad.State
export 
implementation {m : Type -> Type} -> Functor m => Functor (Builder m) where 
  map f x = mkBuilder (map f $ unBuilder x)

export 
implementation {m : Type -> Type} -> Monad m => Applicative (Builder m) where 
    pure x = mkBuilder (lift $ pure x)
    (<*>) f x = mkBuilder (unBuilder f <*> unBuilder x)

export 
implementation {m : Type -> Type} -> Monad m => Monad (Builder m) where 
    (>>=) x f = (mkBuilder (unBuilder x >>= (unBuilder . f)))
    join x = mkBuilder (join $ unBuilder <$> unBuilder x)

export 
implementation MonadTrans (Builder) where 
    lift x = mkBuilder (lift $ lift x)

export  
implementation {m : Type -> Type} -> Monad m => MonadError (List BuilderError) (Builder m) where 
    throwError e = mkBuilder (throwError e)
    catchError x f = mkBuilder (catchError (unBuilder x) (unBuilder . f))
export 
implementation {m : Type -> Type} -> Monad m => MonadState BuilderState (Builder m) where
    get = mkBuilder (lift get)
    put x = mkBuilder (lift $ put x)
export 
implementation {m : Type -> Type} -> Monad m => MonadReader BuilderContext (Builder m) where 
    ask = mkBuilder (lift ask)
    local f x = mkBuilder (local f $ unBuilder x)
export 
implementation {m : Type -> Type} -> Monad m => MonadWriter BuilderLog (Builder m) where 
    tell x = mkBuilder (lift $ tell x)
    listen x = mkBuilder (listen $ unBuilder x)
    pass x = mkBuilder (pass $ unBuilder x)
 


{-
export 
implementation MonadTrans t => MonadBuilder s m => MonadBuilder s (t m) where 
  uniqueId = lift uniqueId
-} 