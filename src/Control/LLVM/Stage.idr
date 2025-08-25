module Control.LLVM.Stage
import Control.Monad.RWS
import Control.Monad.Either
import public Data.LLVM
||| Compilation error types for LLVM operations.
|||
||| Represents various errors that can occur during LLVM compilation,
||| optimization, linking, and execution operations.
public export
data StageError : Type where 
    ||| Error during linking phase
    LinkError : String -> StageError
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
||| Show instance for compilation errors
export
Show StageError where 
    show (LinkError msg) = "Link Error: " ++ msg
    show (AssembleError msg) = "Assemble Error: " ++ msg
    show (CompileError msg) = "Compile Error: " ++ msg
    show (RunError msg) = "Run Error: " ++ msg
    show (OptimizeError msg) = "Optimize Error: " ++ msg
    show (OtherError msg) = "Other Error: " ++ msg


||| Compilation StageContext containing configuration and state.
|||
||| Holds all the information needed for a complete LLVM compilation pipeline,
||| including optimization passes, modules, and file locations.
public export
record StageContext where 
    constructor MkStageContext
    ||| List of optimization passes to apply
    passes : List Pass
    ||| Optional main module file path
    mainModule : Maybe String
    ||| Build directory for intermediate files
    buildDir : String 
    ||| Temporary directory for working files
    tempDir : String 
    ||| Extra LLVM IR files (name, content pairs)
    extraIr : List (String, String) 
    ||| Extra bitcode file paths
    extraBc : List String
    ||| Extra object file paths
    extraObj : List String
    ||| Output file path
    output : String 



export 
record StageState where 
    constructor MkStageState 
record StageWriter where 
    constructor MkStageWriter
defaultContext : StageContext 
defaultState : StageState 
defaultWriter : StageWriter
defaultState = MkStageState
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
    {default Nothing mainModule : Maybe String} -> 
    (buildDir : String) -> 
    {default buildDir tempDir : String} -> 
    {default [] extraIr : List (String, String)} -> 
    {default [] extraBc : List String} -> 
    {default [] extraObj : List String} -> 
    {default "main" output : String} -> 
    StageContext
context {passes} {mainModule} buildDir {tempDir} {extraIr} {extraBc} {extraObj} {output} = 
    MkStageContext passes mainModule buildDir tempDir extraIr extraBc extraObj output

public export
Stage : Type -> Type 
Stage = (EitherT (List StageError) (RWST StageContext StageState StageWriter  IO))

unStage : StageContext -> StageWriter -> StageState -> Stage a -> IO ((Either (List StageError) a), StageWriter, StageState)
unStage r w s m = let 
    r0 = runEitherT m
    r1 = (unRWST r0 r w s)
    in r1
public export
runStage : {auto context : StageContext} -> {default defaultState state : StageState} -> Stage a -> IO (Either (List StageError) a)
runStage {context} {state} m = let 
    r0 = runEitherT m
    r1 = (unRWST r0 context defaultWriter state)
    in do 
        (res, _, _) <- r1
        pure res