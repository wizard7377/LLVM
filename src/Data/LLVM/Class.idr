||| Type classes and utilities for encoding LLVM IR to string representations.
|||
||| This module provides the `Encode` interface for converting LLVM IR data types
||| to their textual representations, along with supporting types and utilities
||| for string manipulation and formatting.
module Data.LLVM.Class
import Data.Walk

import public Control.Monad.Identity

||| Interface for encoding values to a target monoid type.
|||
||| The `Encode` interface allows converting LLVM IR data structures
||| to their string representations or other monoid types suitable
||| for serialization or pretty-printing.
|||
||| @ a The source type to encode
||| @ b The target monoid type (usually VString)
public export
interface Monad m => Monoid b => Encode (m : Type -> Type) (a : Type) (b : Type) where
    ||| Convert a value to its encoded representation
    encode : a -> m b


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

||| Semigroup for VString with space separation.
|||
||| The semigroup operation automatically inserts spaces between
||| concatenated VString values, which is useful for LLVM IR formatting.
public export
Semigroup VString where
    (<+>) (MkVString a) (MkVString b) = MkVString (a ++ " " ++ b)

public export
infixl 8 <++> 
-- Infix operator for VString concatenation without spacing.
-- Unlike the semigroup operation, this concatenates strings directly
-- without inserting spaces, useful for building composite tokens.

||| Concatenate VString values without inserting spaces
public export
(<++>) : VString -> VString -> VString
(MkVString a) <++> (MkVString b) = MkVString (a ++ b)

||| Monoid instance for VString with empty string as neutral element
public export
Monoid VString where
    neutral = MkVString ""

%default covering

||| Intercalate a list of monoid values with a separator.
|||
||| Joins a list of monoid values with the given separator between them,
||| similar to the standard library's intercalate but for any monoid.
|||
||| @ sep The separator to insert between elements
||| @ xs The list of values to join
public export
intercalate : Monoid b => b -> List b -> b 
intercalate sep [] = neutral
intercalate sep [x] = x
intercalate sep (x :: xs) = x <+> sep <+> intercalate sep xs

||| Conditionally apply a function based on Maybe value.
|||
||| If the Maybe value is Nothing, returns the monoid neutral element.
||| If it's Just x, applies the function to x.
|||
||| @ f The function to apply to the value
||| @ mx The Maybe value to check
public export
writeIf : Monoid b => (a -> b) -> Maybe a -> b
writeIf _ Nothing = neutral
writeIf f (Just x) = f x
{- 
||| Conditionally encode a Maybe value.
|||
||| Convenience function that applies encoding only if the value is present.
public export
encodeIf : Encode Identity a b => Maybe a -> b
encodeIf x = writeIf <$> encode x

||| Conditionally encode a Maybe value to VString.
|||
||| Type-specialized version of encodeIf for VString output.
encodeIf' : Encode Identity a VString => Maybe a -> VString
encodeIf' x = writeIf <$> encode x
-}
||| Add a prefix to a monoid value.
|||
||| Returns a function that prepends the given prefix to its argument.
|||
||| @ p The prefix to add
public export
prefixed : Monoid m => m -> (m -> m)
prefixed p f = p <+> f

||| Add a suffix to a monoid value.
|||
||| Returns a function that appends the given suffix to its argument.
|||
||| @ s The suffix to add
public export
suffixed : Monoid m => m -> (m -> m)
suffixed s f = f <+> s

||| Encode Identity Maybe values by encoding Just values and ignoring Nothing.
|||
||| This instance allows Maybe types to be encoded directly, where
||| Nothing becomes the neutral element and Just x becomes encode x.
public export
{a, b : Type} -> Encode Identity a b => Encode Identity (Maybe a) b where 
    encode Nothing = pure neutral
    encode (Just x) = encode x

||| Join a list of encodable values with a separator.
|||
||| Encodes each value in the list and joins them with the given separator.
|||
||| @ sep The separator string to use between elements
||| @ vs The list of values to encode and join
public export
seperated : {a : Type} -> Encode Identity a VString => VString -> List a -> Identity VString
seperated sep vs = pure $ intercalate sep (map (runIdentity . encode) vs)
||| Join a list of encodable values with space separation
public export
spaced : {a : Type} -> Encode Identity a VString => List a -> Identity VString
spaced vs = pure $ intercalate " " (map (runIdentity . encode) vs)

||| Join a list of VString values with space separation
public export 
spaced' : List VString -> VString
spaced' vs = intercalate " " vs

||| Identity encoding for monoid types
|||
||| Any monoid can be encoded as itself, providing a trivial encoding.
public export
{a : Type} -> Monoid a => Encode Identity a a where 
    encode x = pure x

||| Encode Identity lists with comma separation (named instance)
|||
||| Provides comma-separated encoding of lists, useful for function arguments
||| and other comma-delimited LLVM IR constructs.
public export
[each] {a : Type} -> Encode Identity a VString => Encode Identity (List a) VString where 
    encode [] = pure ""
    encode xs = pure $ intercalate ", " (map (runIdentity . encode) xs)

||| Encode Identity lists with no separation (named instance)
|||
||| Concatenates list elements without any separator.
public export
[nosep] {a : Type} -> Encode Identity a VString => Encode Identity (List a) VString where 
    encode [] = pure ""
    encode xs = pure $ intercalate "" (map (runIdentity . encode) xs)

public export
[spacing] {a : Type} -> Encode Identity a VString => Encode Identity (List a) VString where 
    encode [] = pure ""
    encode xs = pure $ intercalate " " (map (runIdentity . encode) xs)
||| Encode Identity lists with newline separation (named instance)
|||
||| Each list element appears on its own line.
public export
[lined] {a : Type} -> Encode Identity a VString => Encode Identity (List a) VString where 
    encode [] = pure ""
    encode xs = pure $ intercalate "\n" (map (runIdentity . encode) xs)

||| Encode Identity lists with newline and tab separation (named instance)
|||
||| Each list element appears on its own line, indented with a tab.
public export 
[tabbed] {a : Type} -> Encode Identity a VString => Encode Identity (List a) VString where 
    encode [] = pure ""
    encode xs = pure $ intercalate "\n\t" (map (runIdentity . encode) xs)

||| Encode Identity Maybe values, ignoring Nothing (named instance)
|||
||| Alternative Maybe encoding that produces empty string for Nothing
||| instead of using the neutral element.
public export
[just] {a : Type} -> Encode Identity a VString => Encode Identity (Maybe a) VString where 
    encode Nothing = pure ""
    encode (Just x) = encode x

||| Convert Show instances to VString
|||
||| Convenience function that combines show and go to create VString values
||| from any type with a Show instance.
public export
vshow : Show a => a -> VString
vshow x = go (show x)

||| String encoding to VString
|||
||| Direct encoding of strings to VString via go.
public export 
Encode Identity String VString where
    encode s = pure (go s)

||| Type-specialized encoding function
|||
||| Convenience function with explicit VString return type.
public export
encode' : {a : Type} -> Encode Identity a VString => a -> Identity VString
encode' = encode
{- 
||| Convert VString back to String
public export
Walk VString String where
    go (MkVString s) = s
-}
||| Convert String to VString (redundant with go)
public export
toVString : String -> VString
toVString s = MkVString s

||| Encode Identity natural numbers using vshow
public export 
Encode Identity Nat VString where
    encode n = pure (vshow n)


||| Show instance for VString
|||
||| Extracts the underlying string for display.
public export 
Show VString where
    show (MkVString s) = s