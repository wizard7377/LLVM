module Control.LLVM.Stage
import public Control.Monad.RWS
import public Control.Monad.Either
import public Data.LLVM
import public Data.Table
import public System.File
import public System.Directory
import System
||| Compilation error types for LLVM operations.
|||
||| Represents various errors that can occur during LLVM compilation,
||| optimization, linking, and execution operations.
public export
data StageError : Type where 
    ||| Error during linking phase
    LinkError : String -> StageError
    ||| Convert error 
    ConvertError : String -> StageError
    
    ||| Error during assembly phase
    AssembleError : String -> StageError
    ||| Error during compilation phase  
    CompileError : String -> StageError
    ||| Error during execution phase
    RunError : String -> StageError
    ||| Error during optimization phase
    OptimizeError : String -> StageError
    ||| General error not fitting other categories
    OtherError : String -> StageError
    ||| File error 
    StageFileError : FileError -> StageError
||| Show instance for compilation errors
export
Show StageError where 
    show (LinkError msg) = "Link Error: " ++ msg
    show (AssembleError msg) = "Assemble Error: " ++ msg
    show (ConvertError msg) = "Convert Error: " ++ msg
    show (CompileError msg) = "Compile Error: " ++ msg
    show (RunError msg) = "Run Error: " ++ msg
    show (OptimizeError msg) = "Optimize Error: " ++ msg
    show (OtherError msg) = "Other Error: " ++ msg
    show (StageFileError msg) = "Stage file Error: " ++ show msg


||| Compilation StageContext containing configuration and state.
|||
||| Holds all the information needed for a complete LLVM compilation pipeline,
||| including optimization passes, modules, and file locations.
public export
record StageContext where 
    constructor MkStageContext
    ||| List of optimization passes to apply
    passes : List Pass
    ||| Verbosity 
    currentVerbosity : Int
    skipNative : Bool
    ||| Optional main module file path
    mainModule : Maybe String
    ||| Build directory for intermediate files
    buildDir : String 
    ||| Temporary directory for working files
    tempDir : String 
    ||| Extra bitcode file paths
    extraBc : Table Code
    ||| Extra object file paths
    extraObj : Table Code
    ||| Output file path
    output : String 



export 
record StageState where 
    constructor MkStageState 
    uid : Int
record StageWriter where 
    constructor MkStageWriter
defaultContext : StageContext 
defaultState : StageState 
defaultWriter : StageWriter
defaultState = MkStageState 1
defaultWriter = MkStageWriter
export 
Semigroup StageWriter where 
    (MkStageWriter) <+> (MkStageWriter) = MkStageWriter
export 
Monoid StageWriter where
    neutral = defaultWriter

public export
context : 
    {default [Level 2] passes : List Pass} -> 
    {default 10 verbosity : Int} ->
    {default True skipNative : Bool} ->
    {default Nothing mainModule : Maybe String} -> 
    (buildDir : String) -> 
    {default buildDir tempDir : String} -> 
    {default [] extraBc : Table Code} -> 
    {default [] extraObj : Table Code} -> 
    {default "main" output : String} -> 
    StageContext
context {passes} {verbosity} {skipNative} {mainModule} buildDir {tempDir} {extraBc} {extraObj} {output} = 
    MkStageContext passes verbosity skipNative mainModule buildDir tempDir extraBc extraObj output

export
data Stage : Type -> Type where 
  MkStage : (EitherT (List StageError) (RWST StageContext StageWriter StageState IO)) a -> Stage a

unStage : Stage a -> (EitherT (List StageError) (RWST StageContext StageWriter StageState  IO)) a
unStage (MkStage m) = m
public export
runStage : {context : StageContext} -> {default defaultState state : StageState} -> Stage a -> IO (Either (List StageError) a)
runStage {context} {state} (MkStage m) = let 
    r0 = runEitherT m
    r1 = (unRWST r0 context state defaultWriter)
    in do 
        _ <- createDir context.tempDir
        _ <- createDir context.buildDir
        (res, _, _) <- r1
        pure res

export
mkStage : (EitherT (List StageError) (RWST StageContext StageWriter StageState  IO)) a -> Stage a
mkStage = MkStage

 
export 
Functor Stage where 
    map f (MkStage m) = MkStage (map f m)
export
Applicative Stage where 
    pure x = MkStage (pure x)
    (MkStage f) <*> (MkStage x) = MkStage (f <*> x)
export
Monad Stage where
    (MkStage m) >>= f = mkStage (m >>= (unStage . f))

export 
HasIO Stage where 
    liftIO io = MkStage (liftIO io)

export 
MonadReader StageContext Stage where 
    ask = MkStage (ask)
    local f (MkStage m) = MkStage (local f m)
export
MonadState StageState Stage where 
    get = MkStage (lift get)
    put s = MkStage (put s)
export 
MonadWriter StageWriter Stage where 
    tell w = MkStage (tell w)
    listen (MkStage m) = MkStage (listen m)
    pass (MkStage m) = MkStage (pass m)
export
MonadError (List StageError) Stage where 
    throwError err = MkStage (throwError err)
    catchError (MkStage m) handler = MkStage (catchError m (unStage . handler))

export 
Alternative Stage where 
    empty = MkStage (empty)
    (MkStage x) <|> (MkStage y) = MkStage (x <|> y)

-- export 
-- Traversable Stage where 
    -- traverse f (MkStage m) = mkStage (traverse (unStage . f) m)
export
sequenceStage : List (Stage a) -> Stage (List a)
sequenceStage (x :: xs) = do 
  y <- x
  ys <- sequenceStage xs
  pure (y :: ys)
sequenceStage [] = pure []
export
traverseStage : (a -> Stage b) -> List a -> Stage (List b)
traverseStage f t = sequenceStage (f <$> t)
export 
runCmd : {default 10 verb : Int} -> String -> Stage (String, Int)
runCmd {verb} cmd = do
    context <- ask
    _ <- (when $ verb <= context.currentVerbosity) (putStrLn $ "Running command: " ++ cmd)
    (sout, res) <- run cmd
    pure (sout, res)


export 
showMsg : {default 10 verb : Int} -> String -> Stage ()
showMsg {verb} msg = do
    context <- ask
    _ <- (when $ verb <= context.currentVerbosity) (putStrLn msg)
    pure () 

export 
getUid : Stage Int
getUid = do
    st <- get
    modify {uid $= (+ 1)}
    pure st.uid

export 
newFile : Stage String
newFile = do 
    context <- ask
    n <- getUid 
    let f0 = "TEMP_" ++ show n 
    let f = context.tempDir ++ "/" ++ f0
    pure f
export
asFile : Code -> Stage String
asFile c = do 
    context <- ask
    f <- newFile
    Right f' <- openFile f ReadWrite | Left e => throwError [StageFileError e]
    closeFile f' 
    pure f
