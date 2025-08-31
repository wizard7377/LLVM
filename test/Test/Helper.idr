module Test.Helper

import Data.List
import Data.String
import Data.LLVM.Class
import System.File.ReadWrite
import Control.LLVM
import Data.LLVM.Builders
import Data.LLVM.IR.Program
import Data.LLVM.Write
import System.Escape
import Data.LLVM.IR.Core
import Control.Monad.Identity
import System.Signal
import Control.ANSI


public export 
seperate : IO a -> IO ()
--seperate m = fork (m $> ()) $> ()
seperate m = m $> ()
-- import Data.MonadIO
showGreen : String -> String
showGreen str = show $ colored Green str
showRed : String -> String
showRed str = show $ colored Red str
public export
debugTest : {a : Type} -> Encode ATM a VString => String -> a -> IO ()
debugTest name value = seperate $ do
    let str : VString = runATM (encode value)
    let result = show str
    putStrLn $ "\n\nRunning test:\n=============\n" ++ name
    putStrLn $ "Value:\n=============\n\n" ++ result
    putStrLn "\n\nTest completed.\n\n"

public export 
removeSpaces : String -> String
removeSpaces = pack . (filter (not . isSpace)) . unpack
public export
debugFileTest : {a : Type} -> Encode ATM a VString => String -> a -> IO ()
debugFileTest file value = seperate $ do
    let str : VString = runATM (encode value)
    let result = show str
    _ <- writeFile ("generated/" ++ (removeSpaces file) ++ ".ll") result
    putStrLn $ "Test output written to " ++ file

public export
debugCompile : String -> LModule -> IO ()
debugCompile file value = seperate $ do 
    putStrLn $ "Compiling " ++ file ++ "..."
    let context' = context ("./generated/" ++ file ++ "/llvm") {output = "main" ++ file}
    res <- runStage {context=context'} $ compile (bytecode [(file, value)]) 
    case res of 
        Right res' => do 
          Right _ <- (codeToFile res' ("main" ++ file)) | Left _ => ?todo2 
          putStrLn $ "Test output written to " ++ ("main" ++ file) ++ "\n\n" ++ showGreen "TEST SUCCEEDED" ++ "\n\n"
        Left e => putStrLn $ (showRed "TEST FAILED") ++ "\nTEST " ++ file ++ " FAILED WITH" ++ show e ++ "\n\n"
    pure ()

public export
debugRun : String -> LModule -> IO ()
debugRun file value = seperate $ do
    ?todo
    --let context' = context "generated/llvm/"
    --res <- exec {context = context'} (bytecode {modules = [( file, value)]})  
    --pure ()
    --putStrLn $ "Test output written to " ++ res
public export
encodeTest : {a : Type} -> Encode ATM a VString => String -> a -> String -> IO ()
encodeTest name value expected = seperate $ do
    let str : VString = runATM (encode value)
    let result = show str
    if result == expected 
      then
          putStrLn $ "Test " ++ name ++ " passed."
      else
          putStrLn $ "Test " ++ name ++ " failed" ++ ", got:" ++ result
public export
encodeIOTest : (m : Type -> Type) -> {auto e : Type} -> ({b : Type} -> EncodeIO e m a b => String -> a -> IO ())
encodeIOTest m {e} name value = seperate $ do
    putStrLn $ "Running test: " ++ name
    
    res : Either e b <- encodeIO {e = e} {m = m} value
    x <- handleManyCollectedSignals (limit 16)
    case res of
      Right result => putStrLn $ showGreen $ "Test " ++ name ++ " passed."
      Left e => putStrLn $ showRed $ "Test " ++ name ++ " failed with error: " ++ show e

public export
encodeFCMTest : {default CPtr b : Type} -> EncodeIO FCError FCM a b => String -> a -> IO ()
encodeFCMTest {b} name value = encodeIOTest FCM {e = FCError} {b = b} name value

public export
encodeFCMTest' : EncodeIO FCError FCM LModule CPtr => String -> LModule -> IO ()
encodeFCMTest' = encodeFCMTest {b = CPtr}

public export 
tempModule : LModule
tempModule = emptyModule
