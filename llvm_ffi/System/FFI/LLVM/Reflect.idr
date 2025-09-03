module System.FFI.LLVM.Reflect

import Language.Reflection
import System.FFI
%language ElabReflection

%default covering
public export
record FParam where 
    constructor MkFParam
    count : Count 
    piInfo : PiInfo TTImp 
    name : Maybe Name 
    type : TTImp 
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

mapFArg : Monad m => (TTImp -> m TTImp) -> FParam -> m FParam
mapFArg f x = do
  y <- f x.type
  let z = { type := y } x
  pure z
mapFArg2 : Monad m => (TTImp -> m (TTImp, a)) -> FParam -> m (FParam, a)
mapFArg2 f x = do
  (y, i) <- f x.type
  let z = { type := y } x
  pure (z, i) 
breakDown' : TTImp -> Elab (Maybe (FParam, TTImp))
breakDown' (IPi fc count piInfo name ty0 ty1) = 
    pure $ Just (MkFParam count piInfo name ty0 , ty1)
breakDown' _ = pure Nothing
export 
breakDown : TTImp -> Elab Curried 
breakDown ty = do
    case !(breakDown' ty) of
        Just (param, rest) => do
            MkCurried params final <- breakDown rest
            pure (MkCurried (param :: params) final)
        Nothing => pure (MkCurried [] ty)
  
egetHead : TTImp -> Elab TTImp 
egetHead d = do 
  MkCurried args _ <- breakDown d
  case args of
    (x :: xs) => pure x.type
    [] => fail "No args to get head from"
lowerFor : String -> (ty : TTImp) -> Name -> Decl
lowerFor s ty n = let 
    ex = ForeignFn [(IPrimVal emptyFC $ Str s)]
    decl = IClaim (MkFCVal emptyFC (MkIClaimData MW Export [ex] $ (MkTy emptyFC (MkFCVal emptyFC n) ty)))
  in decl
  
lowerDecl : (ty : TTImp) -> Name -> Decl
lowerDecl t n = IClaim (MkFCVal emptyFC (MkIClaimData MW Export [] $ (MkTy emptyFC (MkFCVal emptyFC n) t)))
lowerRef : Name -> TTImp
lowerRef = IVar emptyFC 
lowerLam : (ty : TTImp) -> (bo : TTImp -> Elab TTImp) -> Elab TTImp 
lowerLam t f = do 
  n <- genSym "lamArg"
  bo' <- f (lowerRef n)
  pure $ ILam emptyFC MW ExplicitArg (Just n) t bo'
lowerApp : (f : TTImp) -> (x : TTImp) -> TTImp
lowerApp f x = IApp emptyFC f x
  
lowerDef : Name -> TTImp -> Decl
lowerDef f x = IDef emptyFC f [PatClause emptyFC (lowerRef f) x]
lowArrow : FParam -> TTImp -> TTImp
lowArrow (MkFParam count piInfo name ty0) ty1 = IPi emptyFC count piInfo name ty0 ty1
lowerSpec : TTImp -> TTImp -> TTImp 
lowerSpec i a = IAutoApp emptyFC i a
breakUp : Curried -> TTImp
breakUp (MkCurried (x :: xs) res) = lowArrow x (breakUp (MkCurried xs res))
breakUp (MkCurried [] res) = res

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

hold : Elab a -> Elab (Maybe a)
hold = catch
export 
primOfName : Name -> Elab Name
primOfName n = do 
    let n' = "prim__" ++ (show $ dropNS n)
    prim_n <- inCurrentNS $ !(genSym n')
    pure prim_n

public export 
interface Boxing (peak : Type) where
  box : AnyPtr -> peak
  unbox : peak -> AnyPtr
    
public export
{a : Type} -> Boxing (Ptr a) where 
  box p = prim__castPtr p
  unbox p = prim__forgetPtr p

public export 
Boxing AnyPtr where
  box p = p
  unbox p = p

||| Decider is of the form `(a -> (b, a -> Elab b))` or, for inverses `(b -> (a, b -> Elab a))`
Decider : Type
Decider = ((a : TTImp) -> (Elab (TTImp, (TTImp -> Elab TTImp))))
interface Decided (t : Type) where
  decide : t -> Decider

 
LiftDecider = (a : Type ** b : Type ** a -> b )
LiftDecider1 : {k : Type} -> Type
LiftDecider1 {k} = (a : k -> Type ** b : k -> Type ** (x : k) -> a x -> b x)
implementation Decided Decider where
  decide = id
emptyDecider : Decider
emptyDecider x = empty
unliftDecider : LiftDecider -> Decider 
unliftDecider (ty0 ** ty1 ** f) x = do
  v <- check {expected = ty0} x

  let f' : TTImp -> Elab TTImp = \x => do
    x' <- check {expected = ty0} x
    y <- quote (f x')
    _ <- check {expected = ty1} y
    pure y
  pure (!(quote ty1), f')
unliftDecider1 : {k : Type} -> LiftDecider1 {k = k} -> {x : k} -> Decider
unliftDecider1 {k} (a ** b ** f) {x} e = do
  v <- check {expected = a x} e
  let f' : TTImp -> Elab TTImp = \y => do 
    y' <- check {expected = a x} y
    quote (f x y')
  pure (!(quote $ b x), f')

implementation Decided LiftDecider where 
  decide = unliftDecider
--implementation {k : Type} -> Decided (LiftDecider1 {k = k}) where 
  --decide = unliftDecider1
altDecide : Decider -> Decider -> Decider
altDecide a b input = (a input) <|> (b input)
idd : Type -> LiftDecider 
idd t = (t ** t ** id)
choiceDecide : {default id t : Decider -> Decider} -> List Decider -> Decider 
choiceDecide {t} i = foldl altDecide emptyDecider (t <$> i)
choiceDecide' : {default id t : Decider -> Decider} -> {d : Type} -> Decided d => List d -> Decider 
choiceDecide' {t} x = choiceDecide (t . decide <$> x)
makeForeign : (Name -> String) -> Claimed -> Elab (Name, Decl)
makeForeign header claim = do 
    prim_n <- primOfName (name claim)
    let decl = lowerFor (header $ name claim)  (breakUp $ !(breakDown $ ty claim))(prim_n)
    pure (prim_n, decl)

public export
traverseElab : (a -> Elab b) -> List a -> Elab (List b)
traverseElab f (x :: xs) = do
  y <- f x
  ys <- traverseElab f xs
  pure (y :: ys) 
traverseElab f [] = pure []
whine : {default "No value whine" msg : String} -> Elab (Maybe a) -> Elab a
whine {msg} x = case !x of
  Just y => pure y
  Nothing => fail msg
makeLifted : (argDec : Decider) -> (resDec : Decider) -> Name -> Claimed -> Elab (List Decl)
 
applyEach : List (FParam, TTImp -> Elab TTImp) -> Name -> Elab TTImp 
applyEach ((t, f) :: fs) g = do 
  g' <- applyEach fs g
  lowerLam (t.type) (\x => lowerApp g' <$> (f x))
applyEach [] g = pure $ IVar emptyFC g
makeLifted decide resDec prim claim = do
  ty0 <- breakDown claim.ty
  let args = ty0.params
  let res' = ty0.result
  maps : List (FParam, TTImp -> Elab TTImp) <- traverseElab (mapFArg2 $ decide) args
  (res, t) <- resDec res'
  let declv = lowerDecl (breakUp $ MkCurried (fst <$> maps) $ res) (name claim)
  lam <- applyEach maps prim
  let defs = lowerDef (name claim) $ !(t lam)
  pure ([declv, defs])
  
  where 
    changeArg : TTImp -> Elab (TTImp, TTImp -> Elab TTImp)
    changeArg e = do
      ty0 <- decide $ e
      pure ty0
        
suchThat : (TTImp -> Bool) -> Decider -> Decider 
suchThat p decide input = do 
  (ty, f) <- decide input
  if p ty then 
    pure (ty, f)
    else fail "Condition not satisfied 3"
boxDecide : Decider 
boxDecide input = do 
  logMsg "elab" 4 "Box decide"
  t' <- check {expected = Type} input 
  i <- whine {msg = "No boxing for: " ++ show input} $ search (Boxing t')
  pure (input, \x => pure $ lowerApp !(quote (box @{i})) x)

unboxDecide : Decider 
unboxDecide input = do 
  logMsg "elab" 4 "UnBox decide"
  t' <- check {expected = Type} input 
  i <- whine {msg = "No unboxing for: " ++ show input} $ search (Boxing t')
  pure (input, \x => pure $ lowerApp !(quote (unbox @{i})) x)

primIODecide : Decider 
primIODecide input = do 
  logMsg "elab" 4 "PrimIO decide"
  t' <- check {expected = Type} input 
  if !(egetHead input) == (lowerRef `{ PrimIO }) then  
    pure (input, pure)
    else fail "Not a PrimIO"
easyDecide : (pred : Decider -> Decider) -> Decider
easyDecide pred = choiceDecide' {t = pred} [
        idd Int,
        idd Char,
        idd Double,
        idd Bits8,
        idd Bits16,
        idd Bits32,
        idd Bits64,
        idd String,
        idd AnyPtr,
        idd ()
    ]
simpleDecider : (pred : Decider -> Decider) -> Decider 
simpleDecider pred = choiceDecide {t = pred} [
    easyDecide pred,
    boxDecide
]

cosimpleDecider : (pred : Decider -> Decider) -> Decider 
cosimpleDecider pred = choiceDecide {t = pred} [
    easyDecide pred,
    unboxDecide,
    primIODecide
]

public export
makeForms : (header : Name -> String) -> Decl -> Elab (List Decl)
makeForms header decl = do
  Just c <- undecl decl | Nothing => fail "Not a claim 4"
  (prim, decl0) <- makeForeign header c
  lifted <- makeLifted (simpleDecider id) (cosimpleDecider id) prim c
  pure (decl0 :: lifted)
