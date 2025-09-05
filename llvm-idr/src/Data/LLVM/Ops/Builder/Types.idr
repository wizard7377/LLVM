module Data.LLVM.Ops.Builder.Types
import Control.Monad.RWS
import Control.Monad.Identity
import Control.Monad.Either
import Control.Monad.State
import Data.SortedMap
import Data.LLVM.IR.Core
import Data.LLVM.IR.Core
import Data.LLVM.IR.Program
mutual
    public export 
    record BuilderState where 
        constructor MkBuilderState
        currentId : Nat
        typeInfo : SortedMap Name LType
    public export
    data BuilderError : Type where 
        MismatchedTypes : LType -> LType -> BuilderError
    public export 
    record BuilderEnv where 
        constructor MkBuilderEnv
        checkTypes : Bool
    public export 
    record BuilderLog where 
        constructor MkBuilderLog
        messages : List String
    defaultState : BuilderState
    defaultState = MkBuilderState 0 empty
    defaultEnv : BuilderEnv
    defaultEnv = MkBuilderEnv True
    export 
    data Builder : Type -> Type where 
        MkBuilder : EitherT (List BuilderError) (RWS BuilderEnv BuilderLog BuilderState) a -> Builder a
export
Semigroup BuilderLog where 
    (<+>) (MkBuilderLog m1) (MkBuilderLog m2) = MkBuilderLog (m1 ++ m2)
export
Monoid BuilderLog where 
    neutral = MkBuilderLog []
export
runBuilder : Builder a -> (Either (List BuilderError) a, (BuilderState, BuilderLog))
runBuilder (MkBuilder b) = let 
    r0 = runEitherT b
    r1 = runRWS defaultEnv defaultState r0
    in r1

export 
runBuilder' : Builder a -> Maybe a
runBuilder' b = case (runBuilder b) of
    (Left _, _, _) => Nothing
    (Right a, _, _) => Just a
export
Functor Builder where 
    map f (MkBuilder b) = MkBuilder (map f b)

export
Applicative Builder where
    pure x = MkBuilder (pure x)
    (MkBuilder f) <*> (MkBuilder b) = MkBuilder (f <*> b)

export 
Monad Builder where
    (MkBuilder b) >>= f = MkBuilder (b >>= \x => let MkBuilder b' = f x in b')
export
MonadError (List BuilderError) Builder where
    throwError e = MkBuilder (throwError e)
    catchError (MkBuilder b) h = MkBuilder (catchError b (\e => let MkBuilder b' = h e in b'))
export
MonadState BuilderState Builder where
    get = MkBuilder (get)
    put s = MkBuilder (put s)
export
MonadReader BuilderEnv Builder where
    ask = MkBuilder (ask)
    local f (MkBuilder b) = MkBuilder (local f b)
export
MonadWriter BuilderLog Builder where
    listen (MkBuilder b) = MkBuilder (listen b)
    pass (MkBuilder b) = MkBuilder (pass b)

export 
Semigroup a => Semigroup (Builder a) where
    (MkBuilder b1) <+> (MkBuilder b2) = MkBuilder (b1 <+> b2)
export 
Monoid a => Monoid (Builder a) where
    neutral = pure neutral


export 
Alternative Builder where
    empty = MkBuilder (empty)
    (MkBuilder b1) <|> (MkBuilder b2) = MkBuilder (b1 <|> b2)

