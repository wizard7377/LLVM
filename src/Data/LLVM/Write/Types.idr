module Data.LLVM.Write.Types

import Data.LLVM.IR
import Data.LLVM.Class
import Data.Walk
import Data.LLVM.Class
import Data.LLVM.IR
import Data.LLVM.CC
import Data.LLVM.CC
import public Control.Monad.State
import public Control.Monad.Either 
import public Control.Monad.RWS
import public Data.SortedMap
%default covering

public export
ATM : Type -> Type 
ATM = Identity
export
||| Conditionally encode a Maybe value using writeIf
encodeIf : {a, b : Type} -> Monoid b => Encode ATM a b => Maybe a -> ATM b
encodeIf = writeIf encode

export
||| Helper function to work with VString directly in spaced format
spacedVString : List VString -> VString
spacedVString = intercalate " "
export
||| Helper to sequence monadic encodings and join with spaces
spacedM : List (ATM VString) -> ATM VString  
spacedM xs = pure $ intercalate " " (map runIdentity xs)

export 
MonadIO () ATM where 
    unliftIO m = 
        let res = runIdentity m in pure $ Right res
