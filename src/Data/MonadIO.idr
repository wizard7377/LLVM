module Data.MonadIO 

import Control.Monad.Identity
public export 
interface MonadIO e m where
    unliftIO : m a -> IO (Either e a)

export 
MonadIO e IO where
    unliftIO = map Right

export 
MonadIO e (\x => x) where 
    unliftIO = pure . Right

export 
MonadIO e (Identity) where 
    unliftIO ( x) = pure (Right $ runIdentity x)
