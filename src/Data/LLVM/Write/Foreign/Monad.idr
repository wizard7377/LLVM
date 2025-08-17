module Data.LLVM.Write.Foreign.Monad


import Data.LLVM.Class
import Data.LLVM.IR
import Data.LLVM.CC
import Data.LLVM.CC
import public Control.Monad.State
import public Control.Monad.Either 
import public Data.LLVM.Write.Types
import System
import Data.String
%default covering 

  
public export 
data FCError : Type where 
  NoContext : FCError
  NoModule : FCError
  NoBuilders : FCError
  NoFunctions : FCError 
  EmptyFunction : String -> FCError
  NoScope : String -> FCError
  NoGeneric : FCError
  NoBlock : String -> FCError
  InternalError : String -> FCError

public export 
data Usages : Type where
  UseFunction : Usages 
  UseScope : Name -> Usages
  UseModule : Usages 
  UseBuilder : Usages 
public export 
record WithScope {default String name : Type} {default AnyPtr ref : Type} (me : Type) where 
  constructor MkWithScope
  scope : SortedMap name ref
  value : me
  
export 
{a : Type} -> Cast (WithScope {ref = a} b) b where
  cast (MkWithScope _ v) = v

public export 
Scope : Type -> Type 
Scope t = SortedMap Name t
public export 
record WorkingFunction where 
  constructor MkWorkingFunction 
  args : List (Maybe String)
  blocks : SortedMap String LLVMBlock
  val : LLVMValue
mutual 
  public export 
  record FCState where
    constructor MkFCState
    cMod : Maybe LLVMModule
    cCon : LLVMContext
    cFun : List WorkingFunction   
    cBuilders : List LLVMBuilder 
    scope : Scope (List (FCM LLVMValue))
    level : Int
  -- FIXME: change this so that it depenndtly tracks usages 
  public export 
  data FCM : {auto 0 takes : List Usages} -> {auto 0 gives : List Usages} -> {auto 0 sees : List Usages} -> Type -> Type where 
    MkFCM : EitherT FCError (RWST Int () FCState IO) a -> FCM a

public export 
unFCM : FCM a -> EitherT FCError (RWST Int () FCState IO) a
unFCM (MkFCM m) = m

public export
noScope :  {me : Type} -> {0 ref : Type} -> {name : Type} -> Ord name => me -> WithScope {name = name} {ref = ref} me
noScope {ref} {me} v = MkWithScope {ref = ref} {me = me} empty v

public export 
Functor FCM where
  map f (MkFCM m) = MkFCM (map f m)

public export 
Applicative FCM where
  pure = MkFCM . pure
  (<*>) (MkFCM f) (MkFCM x) = MkFCM (f <*> x)

public export
Monad FCM where 
  m >>= f = MkFCM ((unFCM m) >>= (unFCM . f))

public export
HasIO FCM where 
  liftIO = MkFCM . liftIO

public export 
MonadError FCError FCM where 
  throwError e = MkFCM $ throwError e
  catchError m h = MkFCM $ catchError (unFCM m) (unFCM . h)

public export 
MonadState FCState FCM where 
  get = MkFCM $ get
  put x = MkFCM $ put x

public export 
MonadReader Int FCM where 
  ask = MkFCM $ ask
  local f (MkFCM m) = MkFCM $ local f m
export
scoped : FCM a -> FCM a
scoped f = do
  st <- get
  f' <- f 
  put st
  pure f'
export
initFCState : IO FCState 
initFCState = do
  context <- fromPrim LLVMContextCreate
  pure $ MkFCState Nothing (context) [] [] empty 1

private 
defaultLevel : Int 
defaultLevel = 1
export 
runFCM : {default (-1) level : Int} -> FCM a -> IO (Either FCError a)
runFCM {level} (MkFCM m) = do 
  env <- getEnv "IDRIS_LLVM_VERBOSITY"
  let e = parseInteger {a = Int} =<< env
  st <- initFCState
  let l' : Maybe Int = if level < 0 then Nothing else Just level
  let verb : Int = 
    case l' of 
        Just lv => lv
        Nothing => case e of 
          Just ev => ev
          Nothing => defaultLevel
    
  let r0 = runEitherT m
  let r1 = evalRWST verb st r0
  fst <$> r1
 
export
liftFCM : {0 a : Type} -> PrimIO a -> FCM a
liftFCM x = MkFCM $ liftIO $ fromPrim x
export  
fcm : {0 a : Type} -> PrimIO a -> FCM a
fcm = liftFCM

 
export
inCon : FCM LLVMModule 
inCon = do 
  st <- get
  pure st.cCon
-- TODO: 
-- Perhaps add a way to encode at type level requirements of these
export
castPtr : CPtr -> Ptr t
castPtr p = prim__castPtr p
export
whence : Maybe a -> (a -> FCM b) -> FCM ()
whence Nothing _ = pure ()
whence (Just x) f = (f x) $> ()
public export
changeState : Type 
changeState = FCState -> FCState
export
%inline
putMsg : Int -> String -> FCM ()
putMsg level msg = do 
  st <- ask
  if st > level then 
    liftIO $ putStrLn ("[FCM]@" ++ show level ++ " : " ++ msg)
    else
      pure ()
export
putStep : String -> FCM ()
putStep msg = putMsg 10 ("Step: " ++ msg)

export 
step : {default 10 verb : Int} -> String -> (FCM a -> FCM a)
step {verb} name m = do 
  state <- get
  let level = state.level
  modify $ the changeState ({ level $= (+ 1) }) 
  (putMsg verb ((replicate (cast $ max 0 (level - 1)) '|') ++ ">" ++ " " ++ name))
  r <- m 
  (putMsg verb ((replicate (cast $ max 0 (level - 1)) '|') ++ "<" ++ " " ++ name))
  modify $ the changeState ({ level $= (flip (-) 1) } )
  pure r
export
inMod : FCM LLVMModule 
inMod = do 
  st <- get
  case st.cMod of 
    Nothing => throwError NoModule
    Just m => pure m
export  
withIn : (FCState -> Maybe a) -> FCM a 
withIn f = do 
  st <- get 
  case f st of 
    Nothing => throwError NoGeneric
    Just m => pure m
export
popFun : FCM ?
popFun = do
  st <- get
  case st.cFun of
    [] => throwError NoFunctions 
    (b :: bs) => do
      modify $ the changeState { cFun := bs }
      pure b  
export
pushFun : ? -> FCM ()
pushFun b = do
  st <- get
  modify $ the changeState { cFun := b :: st.cFun }
export
popBuilder : FCM ?
popBuilder = do
  st <- get
  case st.cBuilders of
    [] => throwError NoFunctions 
    (b :: bs) => do
      modify $ the changeState { cBuilders := bs }
      pure b  
export
pushBuilder : ? -> FCM ()
pushBuilder b = do
  st <- get
  modify $ the changeState { cBuilders := b :: st.cBuilders }
export
usingFun : ? -> FCM a -> FCM (a, ?)
usingFun f m = do 
  pushFun f 
  r <- m
  f' <- popFun
  pure (r, f')
export
usingBuilder : ? -> FCM a -> FCM (a, ?)
usingBuilder f m = do 
  pushBuilder f 
  r <- m
  f' <- popBuilder
  pure (r, f')

public export 
encode' : {a : Type} -> Encode FCM a CPtr => (_ : a) -> FCM CPtr 
encode' x = encode {m = FCM} {b = CPtr} x
export
fcmPure : a -> FCM a
fcmPure x = pure x
public export
debugType : CPtr -> FCM () 
debugType ty = do
  name <- liftFCM $ LLVMPrintTypeToString ty
  liftIO $ putStrLn $ "Type: " ++ show name  
export 
Show FCError where
  show NoContext = "No context available"
  show NoModule = "No module available"
  show NoBuilders = "No builders available"
  show NoFunctions = "No functions available"
  show (EmptyFunction f) = "Empty function: " ++ f
  show (NoScope s) = "No scope for: " ++ s
  show (InternalError s) = "BUG IN IDRIS LLVM: FILE BUG REPORT: " ++ s
  show _ = "Other error"
export 
MonadIO FCError FCM where
  unliftIO m = do
    res <- runFCM m
    case res of 
      Right x => pure (Right x)
      Left e => pure (Left e)

export 
pushScope' : Name -> FCM CPtr -> FCM ()
pushScope' name ref = do
  st <- get
  let newScope = SortedMap.insertWith (\x, y => (x ++ y)) name [ref] st.scope
  modify $ the changeState { scope := newScope }
  pure ()
export 
pushScope : Name -> CPtr -> FCM ()
pushScope name ref = pushScope' name (pure ref)

export
getScope : Name -> FCM CPtr
getScope k = do
  st <- get
  x <- case (lookup k st.scope) of
    Just (v :: _) => v
    Just [] => throwError (NoScope $ show k)
    Nothing => throwError (NoScope $ show k)
  pure x

export
popScope : Name -> FCM CPtr 
popScope k = do
  st <- get
  (x, xs) <- case (lookup k st.scope) of
    Just (v :: vs) => do 
      v' <- v 
      pure (v', vs)
    Just [] => throwError (NoScope $ show k)
    Nothing => throwError (NoScope $ show k)
  modify $  the (changeState) $ { scope $= (updateExisting (const xs) k) }
  pure x

export
inScope : Name -> CPtr -> FCM a -> FCM a
inScope name ref m = do 
  pushScope name ref 
  result <- m 
  _ <- popScope name 
  pure result
export
withScope : Name -> (CPtr -> FCM (a, CPtr)) -> FCM a
withScope name f = do 
  ref <- popScope name 
  (r, s) <- f ref 
  pushScope name s
  pure r
export
inBuilder : (LLVMBuilder -> FCM a) -> FCM a 
inBuilder f = do 
  b <- popBuilder 
  r <- f b 
  pushBuilder b
  pure r
public export
getBlock : String -> FCM LLVMBlock
getBlock name = do 
  cf <- popFun 
  case lookup name cf.blocks of 
    Nothing => do 
      cb <- liftFCM $ LLVMAppendBasicBlockInContext !inCon cf.val name 
      let cf' = { blocks $= insert name cb } cf
      pushFun cf'
      pure cb
    Just cb => pure cb
public export 
setBlock : String -> LLVMBlock -> FCM ()
setBlock name cb = do
  cf <- popFun
  case lookup name cf.blocks of
    Nothing => do 
      let cf' = { blocks $= insert name cb } cf
      pushFun cf' 
    Just _ => do 
      let cf' = { blocks $= updateExisting (const cb) name } cf
      pushFun cf'
    