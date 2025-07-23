module Data.LLVM.Class
public export
interface Monoid b => Encode a b where
    encode : a -> b