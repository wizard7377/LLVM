module Data.LLVM.CC.Reflect

import Language.Reflection
import System.FFI
%language ElabReflection

public export
record FParam where 
    constructor MkFParam
    count : Count 
    piInfo : PiInfo TTImp 
    name : Maybe Name 
    type : TTImp 
    result : TTImp 
public export
record Curried where 
    constructor MkCurried
    params : List FParam 
    result : TTImp
public export 
record Claimed where 
    constructor MkClaimed
    count : Count
    vis : Visibility
    opts : List FnOpt
    name : Name 
    ty : TTImp

breakDown' : TTImp -> Elab (Maybe (FParam, TTImp))
breakDown' (IPi fc count piInfo name ty0 ty1) = 
    pure $ Just (MkFParam count piInfo name ty0 ty1, ty1)
breakDown' _ = pure Nothing
export 
breakDown : TTImp -> Elab Curried 
breakDown ty = do
    case !(breakDown' ty) of
        Just (param, rest) => do
            MkCurried params final <- breakDown rest
            pure (MkCurried (param :: params) final)
        Nothing => pure (MkCurried [] ty)

export 
undecl : Decl -> Elab (Maybe Claimed)
undecl (IClaim (MkFCVal fc (MkIClaimData count vis opts (MkTy _ (MkFCVal _ name) ty)))) = 
    pure $ Just (MkClaimed count vis opts name ty)
undecl _ = pure Nothing
public export
isCType : Type -> Bool
isCType String = True 
isCType (Ptr _) = True
isCType (AnyPtr) = True 
isCType (Struct _ _) = True
isCType (Double) = True
isCType (Int) = True
isCType Char = True
isCType Bits8 = True 
isCType Bits16 = True
isCType Bits32 = True
isCType Bits64 = True
isCType () = True 
isCType _ = False

export 
primOfName : Name -> Elab Name
primOfName n = do 
    let n' = "prim__" ++ (show $ dropNS n)
    prim_n <- inCurrentNS $ !(genSym n')
    pure prim_n

public export 
interface Boxing (a : Type) where
    box : a -> AnyPtr
    unbox : AnyPtr -> a
makeForeign : (Name -> String) -> Claimed -> Elab Decl 
makeForeign header claim = do 
    prim_n <- primOfName (name claim)
    let forDecl = ForeignFn [IPrimVal emptyFC (Str $ header (name claim))] 
    let decl = IClaim (MkFCVal emptyFC (MkIClaimData (count claim) Export (opts claim) (MkTy emptyFC (MkFCVal emptyFC prim_n) (result $ !(breakDown $ ty claim)))))
    pure decl

