||| Type classes and utilities for encoding LLVM IR to string representations.
|||
||| This module provides the `Encode` interface for converting LLVM IR data types
||| to their textual representations, along with supporting types and utilities
||| for string manipulation and formatting.
module Data.LLVM.Class
import Data.Walk
import public Data.MonadIO
import public Control.Monad.Identity
import Control.Monad.Either
||| Interface for encoding values to a target monoid type.
|||
||| The `Encode` interface allows converting LLVM IR data structures
||| to their string representations or other monoid types suitable
||| for serialization or pretty-printing.
||| @ m The monad to encode within
||| @ a The source type to encode
||| @ b The target type 
public export
interface Monad m => Encode (m : Type -> Type) (a : Type) (b : Type) where
    ||| Convert a value to its encoded representation
    encode : a -> m b

||| Interface for encoding values to a target monoid type.
|||
||| The `Encode` interface allows converting LLVM IR data structures
||| to their string representations or other monoid types suitable
||| for serialization or pretty-printing.
||| @ m The monad to encode within
||| @ a The source type to encode
||| @ b The target type 
public export
interface Monad m => EncodeIn (context : Type) (m : Type -> Type) (a : Type) (b : Type) where
    encodeIn : context -> a -> m b


||| Value string wrapper for LLVM IR string output.
|||
||| A newtype wrapper around String that provides specialized
||| monoid operations for building LLVM IR text with proper spacing.
public export
data VString = MkVString String 

unVString : VString -> String
unVString (MkVString s) = s
--public export 
--Show VString where
--    show (MkVString s) = s
||| Convert String to VString
public export
Walk String VString where
    go s = MkVString s

public export 
Walk VString String where
    go (MkVString s) = s
||| Show implementation for encodable types
|||
||| Provides a Show instance for any type that can be encoded to VString,
||| allowing easy conversion of LLVM IR types to readable strings.
export
[showEncode] {a : Type} -> Encode Identity a VString => Show a where 
    show s = let 
        r = runIdentity $ encode s
        in unVString r



||| Global hint for string literal conversion
%globalhint
public export
FromString VString where 
    fromString s = MkVString s



||| Show instance for VString
|||
||| Extracts the underlying string for display.
public export 
Show VString where
    show (MkVString s) = s

public export
interface Show e => MonadIO e m => Encode m a b =>  EncodeIO e m a b where
    encodeIO : a -> IO (Either e b) 

export 
{m : Type -> Type} -> {a : Type} -> {b : Type} -> Show e => MonadIO e m => Encode m a b => EncodeIO e m a b where
    encodeIO x = let
        res = (encode {m = m} x)
        in unliftIO res