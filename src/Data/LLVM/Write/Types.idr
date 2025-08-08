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
data FCError : Type where 
  NoContext : FCError
  NoModule : FCError
  NoBuilders : FCError
  NoFunctions : FCError 
  EmptyFunction : String -> FCError
  NoGeneric : FCError

  
public export 
record WithScope {default String name : Type} {default AnyPtr ref : Type} (me : Type) where 
  constructor MkWithScope
  scope : SortedMap name ref
  value : me
  
export 
{a : Type} -> Cast (WithScope {ref = a} b) b where
  cast (MkWithScope _ v) = v
public export 
record FCState where
  constructor MkFCState
  cMod : Maybe LLVMModule
  cCon : LLVMContext
  cFun : List (WithScope {ref = Int} LLVMValue)  
  cBuilders : List (WithScope {name = Register} LLVMBuilder)
  level : Int
public export 
FCM : Type -> Type 
FCM = EitherT FCError (RWST Int () FCState IO)

public export
noScope :  {me : Type} -> {0 ref : Type} -> {name : Type} -> Ord name => me -> WithScope {name = name} {ref = ref} me
noScope {ref} {me} v = MkWithScope {ref = ref} {me = me} empty v

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
