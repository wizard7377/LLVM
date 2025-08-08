module Data.LLVM.CC.Support

import Data.List
import PrimIO
%foreign "C:getAtIndex,array"
prim__getAtIndex : (a : AnyPtr) -> (index : Int) -> PrimIO AnyPtr
%foreign "C:setAtIndex,array"
prim__setAtIndex : (a : AnyPtr) -> (index : Int) -> (value : AnyPtr) -> PrimIO ()
%foreign "C:allocWithLength,array"
prim__allocWithLength : (length : Int) -> PrimIO AnyPtr

%foreign "C:nullPtr,array"
prim__nullPtr : AnyPtr
%foreign "C:isNull,array"
prim__isNull : AnyPtr -> Int




public export 
CString : Type 
CString = Ptr String
private 
withIndex : List a -> List (Int, a)
withIndex xs = zip (map (cast) [0 .. length xs]) xs

export
toCList' : List AnyPtr -> IO AnyPtr
toCList' xs = do
    let len = length xs
    arr <- fromPrim $ prim__allocWithLength $ cast len
    for_ (withIndex xs) $ \(i, x) => fromPrim $ prim__setAtIndex arr i x
    pure arr

export 
toCList : List AnyPtr -> PrimIO AnyPtr 
toCList xs = toPrim $ toCList' xs

export
fromCList' : Int -> AnyPtr -> IO (List AnyPtr)
fromCList' len xs = do
    arr <- traverse (\i => fromPrim $ prim__getAtIndex xs i) (cast <$> [0 .. len - 1])
    pure arr

export
fromCList : Int -> AnyPtr -> PrimIO (List AnyPtr)
fromCList len xs = toPrim $ fromCList' (cast len) xs

fromCString : CString -> (Maybe String)
fromCString s = if 0 == (prim__isNull $ prim__forgetPtr s) then Just (prim__getString s) else Nothing
