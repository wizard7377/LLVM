module Data.LLVM.Write.Text.Types
import Data.LLVM.IR
import Data.LLVM.Class
import Data.Walk
import Data.LLVM.Class
import Data.LLVM.IR
import System.FFI.LLVM
import System.FFI.LLVM
import public Control.Monad.State
import public Control.Monad.Either 
import public Control.Monad.RWS
import public Data.SortedMap
import Data.Table
public export 
record ATState where 
    constructor MkATState
    exprContext : List LStatement 
    blockContext : Table BasicBlock
    generatedId : Int 
    indent : Int

defaultATState : ATState
defaultATState = MkATState [] [] 0 0
public export
ATM : Type -> Type 
ATM = State ATState
export
runATM : ATM a -> a 
runATM m = snd $ runState defaultATState m
||| Semigroup for VString with space separation.
|||
||| The semigroup operation automatically inserts spaces between
||| Semigroup instance for VString that intelligently handles spacing.
|||
||| Concatenates two VString values with a space between them, but only if both
||| strings are non-empty. This prevents extra spaces when concatenating empty
||| strings with non-empty ones, which improves formatting of LLVM IR output.
||| 
||| For concatenation without any spacing, use the <++> operator instead.
public export
Semigroup VString where
    (<+>) (MkVString "") (MkVString b) = MkVString b
    (<+>) (MkVString a) (MkVString "") = MkVString a  
    (<+>) (MkVString a) (MkVString b) = MkVString (a ++ " " ++ b)

public export
infixl 8 <++> 
public export
infixl 8 +++
-- Infix operator for VString concatenation without spacing.
-- Unlike the semigroup operation, this concatenates strings directly
-- without inserting spaces, useful for building composite tokens.

||| Concatenate VString values without inserting spaces
public export
%deprecate
(<++>) : VString -> VString -> VString
(MkVString a) <++> (MkVString b) = MkVString (a ++ b)

public export 
(+++) : VString -> VString -> VString 
(+++) (MkVString a) (MkVString b) = MkVString (a ++ b)
||| Monoid instance for VString with empty string as neutral element
public export
Monoid VString where
    neutral = MkVString ""
export
addExprContext : LStatement -> ATM ()
addExprContext l = modify ({ exprContext $= (++ [l]) })
export 
addBlockContext : Entry BasicBlock -> ATM ()
addBlockContext b = modify ({ blockContext $= (++ [b]) })
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
encodeIf : Encode ATM a b => Maybe a -> b
encodeIf x = writeIf <$> encode x

||| Conditionally encode a Maybe value to VString.
|||
||| Type-specialized version of encodeIf for VString output.
encodeIf' : Encode ATM a VString => Maybe a -> VString
encodeIf' x = writeIf <$> encode x
-}
||| (Add NoWrap) a prefix to a monoid value.
|||
||| Returns a function that prepends the given prefix to its argument.
|||
||| @ p The prefix to add
public export
prefixed : Monoid m => m -> (m -> m)
prefixed p f = p <+> f

||| (Add NoWrap) a suffix to a monoid value.
|||
||| Returns a function that appends the given suffix to its argument.
|||
||| @ s The suffix to add
public export
suffixed : Monoid m => m -> (m -> m)
suffixed s f = f <+> s

||| Encode ATM Maybe values by encoding Just values and ignoring Nothing.
|||
||| This instance allows Maybe types to be encoded directly, where
||| Nothing becomes the neutral element and Just x becomes encode x.
public export
{a, b : Type} -> Monoid b => Encode ATM a b => Encode ATM (Maybe a) b where 
    encode Nothing = pure neutral
    encode (Just x) = encode x

||| Join a list of encodable values with a separator.
|||
||| Encodes each value in the list and joins them with the given separator.
|||
||| @ sep The separator string to use between elements
||| @ vs The list of values to encode and join
public export
seperated : {a : Type} -> Encode ATM a VString => VString -> List a -> ATM VString
seperated sep vs = pure $ intercalate sep (map (runATM . encode) vs)
||| Join a list of encodable values with space separation
||| Filters out empty encoded values to prevent extra spaces
public export
spaced : {a : Type} -> Encode ATM a VString => List a -> ATM VString
spaced vs = pure $ intercalate " " (filter (\s => let MkVString str = s in str /= "") (map (runATM . encode) vs))

||| Join a list of VString values with space separation
||| Filters out empty VStrings to prevent extra spaces
public export 
spaced' : List VString -> VString
spaced' vs = intercalate " " (filter (\(MkVString s) => s /= "") vs)

||| ATM encoding for monoid types
|||
||| Any monoid can be encoded as itself, providing a trivial encoding.
public export
{a : Type} -> Monoid a => Encode ATM a a where 
    encode x = pure x

||| Encode ATM lists with comma separation (named instance)
|||
||| Provides comma-separated encoding of lists, useful for function arguments
||| and other comma-delimited LLVM IR constructs. Filters out empty strings
||| to prevent extra commas and improve formatting.
public export
[each] {a : Type} -> Encode ATM a VString => Encode ATM (List a) VString where 
    encode [] = pure ""
    encode xs = pure $ intercalate ", " (filter (\s => let MkVString str = s in str /= "") (map (runATM . encode) xs))

||| Encode ATM lists with no separation (named instance)
|||
||| Concatenates list elements without any separator.
public export
[nosep] {a : Type} -> Encode ATM a VString => Encode ATM (List a) VString where 
    encode [] = pure ""
    encode xs = pure $ intercalate "" (map (runATM . encode) xs)

public export
[spacing] {a : Type} -> Encode ATM a VString => Encode ATM (List a) VString where 
    encode [] = pure ""
    encode xs = pure $ intercalate " " (filter (\s => let MkVString str = s in str /= "") (map (runATM . encode) xs))
||| Encode ATM lists with newline separation (named instance)
|||
||| Each list element appears on its own line.
public export
[lined] {a : Type} -> Encode ATM a VString => Encode ATM (List a) VString where 
    encode [] = pure ""
    encode xs = pure $ intercalate "\n" (map (runATM . encode) xs)

||| Encode ATM lists with newline and tab separation (named instance)
|||
||| Each list element appears on its own line, indented with a tab.
public export 
%deprecate
[tabbed] {a : Type} -> Encode ATM a VString => Encode ATM (List a) VString where 
    encode [] = pure ""
    encode xs = pure $ intercalate "\n\t" (map (runATM . encode) xs)

||| Encode ATM Maybe values, ignoring Nothing (named instance)
|||
||| Alternative Maybe encoding that produces empty string for Nothing
||| instead of using the neutral element.
public export
[just] {a : Type} -> Encode ATM a VString => Encode ATM (Maybe a) VString where 
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
Encode ATM String VString where
    encode s = pure (go s)

||| Type-specialized encoding function
|||
||| Convenience function with explicit VString return type.
public export
encode' : {a : Type} -> Encode ATM a VString => a -> ATM VString
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

||| Encode ATM natural numbers using vshow
public export 
Encode ATM Nat VString where
    encode n = pure (vshow n)



export
addContext : LStatement -> ATM ()
addContext stmt = modify ({exprContext $= (stmt ::)})
export
popContext : ATM (List LStatement)
popContext = do
    st <- get 
    modify ({exprContext := []})
    pure st.exprContext
export 
popBlockContext : ATM (Table BasicBlock)
popBlockContext = do
    st <- get 
    modify ({blockContext := []})
    pure st.blockContext

export
getUnique : ATM Int 
getUnique = do
    st <- get 
    modify ({generatedId $= (+ 1)})
    pure st.generatedId

export
uniqueId : Int -> String
uniqueId n = ( ("\"___UNIQUE_IDRIS_" ++ show n ++ "___\""))
export
getUid : ATM Name 
getUid = do
    n <- getUnique
    pure $ Local $ uniqueId n
export
getUid' : ATM String 
getUid' = do
    n <- getUnique
    pure $ uniqueId n
export
Semigroup a => Semigroup (ATM a) where 
    x <+> y = (<+>) <$> x <*> y
export
Monoid a => Monoid (ATM a) where
    neutral = pure neutral
export
||| Conditionally encode a Maybe value using writeIf
encodeIf : {a, b : Type} -> Monoid b => Encode ATM a b => Maybe a -> ATM b
encodeIf = writeIf encode

export
||| Helper function to work with VString directly in spaced format
||| Filters out empty VStrings to prevent extra spaces
spacedVString : List VString -> VString
spacedVString vs = intercalate " " (filter (\(MkVString s) => s /= "") vs)
export
||| Helper to sequence monadic encodings and join with spaces
spacedM : List (ATM VString) -> ATM VString  
spacedM xs = intercalate (pure " ") xs

export 
MonadIO () ATM where 
    unliftIO m = 
        let (_, res) = runState defaultATState m in pure $ Right res


public export
inBlock : ATM a -> ATM a
inBlock m = do 
  modify { indent $= (+ 1) }
  r <- m 
  modify { indent $= (\x => x - 1) }
  pure r
  
getTabs : Int -> VString 
getTabs 0 = ""
getTabs x = if x > 0 then "\t" +++ (getTabs (x - 1)) else ""
public export
newline : ATM VString
newline = do 
    st <- get 
    let start = getTabs st.indent
    pure ("\n" +++ start)

  
public export
paragraph : {a : Type} -> Encode ATM a VString => List a -> ATM VString
paragraph (x :: xs) = do 
  nl <- newline 
  rs <- paragraph xs
  r <- encode x
  pure (r +++ nl +++ rs)
paragraph [] = newline

public export 
listing : {a : Type} -> Encode ATM a VString => {default "" note : VString} -> List a -> ATM VString
listing {note} (x :: xs) = do 
  r <- encode x
  rs <- listing xs 
  pure (r +++ "," <+> note <+> rs)
listing {} [] = pure ""
