module System.FFI.LLVM.Macros

import Language.Reflection
import System.FFI.LLVM.Reflect
%language ElabReflection




llvmLib = "libLLVM-20"
lCore : String -> {default "llvm-c/Core.h" header : String} -> String 
lCore s {header} = "C:" ++ s ++ ", " ++ llvmLib ++ ", " ++ header


mkPrim : String -> Decl -> Elab (Name, List Decl)
mkPrim header (IClaim 
        (MkFCVal fc 
        (MkIClaimData count _ ops 
        (MkTy tc 
        (MkFCVal nc name) value)))) = 
            pure $ (name, pure (IClaim 
            (MkFCVal fc 
            (MkIClaimData count Export (ops ++ added)
            (MkTy tc 
            (MkFCVal nc name) value)))))
    where 
        added : List FnOpt 
        added = [ForeignFn [IPrimVal nc (Str $ lCore (show name) {header = header})]]

mkPrim _ _ = fail ""
mkLift : String -> Decl -> Elab (List Decl)
mkLift header (IClaim 
        (MkFCVal fc 
        (MkIClaimData count _ ops 
        (MkTy tc 
        (MkFCVal nc name) value)))) = 
            pure $ pure (IClaim 
            (MkFCVal fc 
            (MkIClaimData count Export (ops ++ added)
            (MkTy tc 
            (MkFCVal nc name) value))))
    where 
        added : List FnOpt 
        added = [ForeignFn [IPrimVal nc (Str $ lCore (show name) {header = header})]]

 
mkLift _ _ = fail ""

getArgs : TTImp -> Elab (List (Type, Type)) 

changeArg : (ty0 : Type) -> (ty1 : Type) -> List (Type, Type) -> Elab (ty0 -> ty1)
public export
fDecl' : String -> Decl -> Elab (List Decl)
fDecl' header 
    (IClaim 
        (MkFCVal fc 
        (MkIClaimData count _ ops 
        (MkTy tc 
        (MkFCVal nc name) value)))) = 
            pure $ pure (IClaim 
            (MkFCVal fc 
            (MkIClaimData count Export (ops ++ added)
            (MkTy tc 
            (MkFCVal nc name) value))))
    where 
        added : List FnOpt 
        added = [ForeignFn [IPrimVal nc (Str $ lCore (show name) {header = header})]]

fDecl' _ dec = fail ("Bad fDecl")
public export
fDecl : {default "llvm-c/Core.h" header : String} -> List Decl -> Elab (List Decl)
fDecl {header} decls = map {f=Elab} concat $ sequence $ fDecl' header <$> decls

public export
decl : {default "llvm-c/Core.h" header : String} -> List Decl -> Elab ()
decl {header} decls = do
{- 
    forms <- traverseElab (makeForms (\n => lCore (show $ dropNS n) {header = header})) decls 
    let forms' : List Decl = concat forms
    declare forms' 

-} 
    decls' <- fDecl {header} decls
    declare decls'

