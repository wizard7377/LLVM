module Test.Helper

import Data.List
import Data.String
import Data.LLVM.Class
import System.File.ReadWrite
import System.LLVM
import Data.LLVM.Builders
import Data.LLVM.Program
import System.Escape
import Data.LLVM.Core
import Control.Monad.Identity
import Control.ANSI
showGreen : String -> String
showGreen str = show $ colored Green str
showRed : String -> String
showRed str = show $ colored Red str
public export
debugTest : {a : Type} -> Encode Identity a VString => String -> a -> IO ()
debugTest name value = do
    let str : VString = runIdentity (encode value)
    let result = show str
    putStrLn $ "\n\nRunning test:\n=============\n" ++ name
    putStrLn $ "Value:\n=============\n\n" ++ result
    putStrLn "\n\nTest completed.\n\n"

public export 
removeSpaces : String -> String
removeSpaces = pack . (filter (not . isSpace)) . unpack
public export
debugFileTest : {a : Type} -> Encode Identity a VString => String -> a -> IO ()
debugFileTest file value = do
    let str : VString = runIdentity (encode value)
    let result = show str
    _ <- writeFile ("generated/" ++ (removeSpaces file) ++ ".ll") result
    putStrLn $ "Test output written to " ++ file

public export
debugCompile : String -> LModule -> IO ()
debugCompile file value = do 
    putStrLn $ "Compiling " ++ file ++ "..."
    let context' = context "./generated/llvm/" {output = "main" ++ file}
    res <- compile {context = context'} (bytecode {modules = [( file, value)]}) 
    case res of 
        Right res' => putStrLn $ "Test output written to " ++ res' ++ "\n\n" ++ showGreen "TEST SUCCEEDED" ++ "\n\n"
        Left e => putStrLn $ (showRed "TEST FAILED") ++ "\nTEST " ++ file ++ " FAILED WITH" ++ show e ++ "\n\n"
    pure ()

public export
debugRun : String -> LModule -> IO ()
debugRun file value = do
    let context' = context "generated/llvm/"
    res <- exec {context = context'} (bytecode {modules = [( file, value)]})  
    pure ()
    --putStrLn $ "Test output written to " ++ res
public export
encodeTest : {a : Type} -> Encode Identity a VString => String -> a -> String -> IO ()
encodeTest name value expected = do
    let str : VString = runIdentity (encode value)
    let result = show str
    if result == expected 
      then
          putStrLn $ "Test " ++ name ++ " passed."
      else
          putStrLn $ "Test " ++ name ++ " failed. Expected: " ++ expected ++ ", got: " ++ result