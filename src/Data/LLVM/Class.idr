module Data.LLVM.Class
public export
interface Monoid b => Encode a b where
    encode : a -> b


public export
data VString = MkVString String 
--public export 
--Show VString where
--    show (MkVString s) = s
public export
Cast String VString where
    cast s = MkVString s
export
[showEncode] Encode a VString => Show a where 
    show s = let 
        (MkVString r) = encode s
        in cast r


%globalhint
public export
FromString VString where 
    fromString s = MkVString s

public export
Semigroup VString where
    (<+>) (MkVString a) (MkVString b) = MkVString (a ++ " " ++ b)

public export
infixl 8 <++> 
public export
(<++>) : VString -> VString -> VString
(MkVString a) <++> (MkVString b) = MkVString (a ++ b)
public export
Monoid VString where
    neutral = MkVString ""

%default covering
public export
intercalate : Monoid b => b -> List b -> b 
intercalate sep [] = neutral
intercalate sep [x] = x
intercalate sep (x :: xs) = x <+> sep <+> intercalate sep xs

public export
writeIf : Monoid b => (a -> b) -> Maybe a -> b
writeIf _ Nothing = neutral
writeIf f (Just x) = f x
public export
encodeIf : Encode a b => Maybe a -> b
encodeIf = writeIf encode 
encodeIf' : Encode a VString => Maybe a -> VString
encodeIf' = writeIf encode 
public export
prefixed : Monoid m => m -> (m -> m)
prefixed p f = p <+> f
public export
suffixed : Monoid m => m -> (m -> m)
suffixed s f = f <+> s
public export
Encode a b => Encode (Maybe a) b where 
    encode Nothing = neutral
    encode (Just x) = encode x
public export
seperated : Encode a VString => VString -> List a -> VString
seperated sep vs = intercalate sep (map encode vs)
public export
spaced : Encode a VString => List a -> VString
spaced vs = intercalate " " (map encode vs)

public export 
spaced' : List VString -> VString
spaced' vs = intercalate " " vs
public export
Monoid a => Encode a a where 
    encode = id

public export
[each] Encode a VString => Encode (List a) VString where 
    encode [] = ""
    encode xs = intercalate "," (map encode xs)
public export
[nosep] Encode a VString => Encode (List a) VString where 
    encode [] = ""
    encode xs = intercalate "" (map encode xs)

public export
[lined] Encode a VString => Encode (List a) VString where 
    encode [] = ""
    encode xs = intercalate "\n" (map encode xs)

public export 
[tabbed] Encode a VString => Encode (List a) VString where 
    encode [] = ""
    encode xs = intercalate "\n\t" (map encode xs)
public export
[just] Encode a VString => Encode (Maybe a) VString where 
    encode Nothing = ""
    encode (Just x) = encode x

public export
vshow : Show a => a -> VString
vshow x = cast (show x)

public export 
Encode String VString where
    encode s = cast s

public export
encode' : Encode a VString => a -> VString
encode' = encode

public export
Cast VString String where
    cast (MkVString s) = s

public export
toVString : String -> VString
toVString s = MkVString s

%defaulthint
public export
Encode a b => Encode b c => Encode a c where
    encode x = let 
        r0 : b = encode x
        in encode r0
public export 
Encode Nat VString where
    encode n = vshow n

public export 
Show VString where
    show (MkVString s) = s