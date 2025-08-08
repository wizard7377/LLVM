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
  (putMsg verb ((replicate (cast $ max 0 level) '>') ++ " " ++ name))
  r <- m 
  (putMsg verb ((replicate (cast $ max 0 level) '<') ++ " " ++ name))
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
export
initFCState : IO FCState 
initFCState = do
  context <- fromPrim LLVMContextCreate
  pure $ MkFCState Nothing (context) [] [] 1

private 
defaultLevel : Int 
defaultLevel = 1
export 
runFCM : {default (-1) level : Int} -> FCM a -> IO (Either FCError a)
runFCM {level} m = do 
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
inCon : FCM LLVMModule 
inCon = do 
  st <- get
  pure st.cCon
export
liftFCM : {0 a : Type} -> PrimIO a -> FCM a
liftFCM x = liftIO $ fromPrim x
export  
fcm : {0 a : Type} -> PrimIO a -> FCM a
fcm = liftFCM

export
scoped : FCM a -> FCM a
scoped f = do
  st <- get
  f' <- f 
  put st
  pure f'
public export
[fcmIO] HasIO FCM where 
  liftIO = liftIO
public export
[fcmMonad] Monad FCM
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
  show _ = "Other error"
export 
MonadIO FCError FCM where
  unliftIO m = do
    res <- runFCM m
    case res of 
      Right x => pure (Right x)
      Left e => pure (Left e)
