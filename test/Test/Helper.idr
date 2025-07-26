module Test.Helper

import Data.List
import Data.String
import Data.LLVM.Class
import System.File.ReadWrite
public export
debugTest : Encode a VString => String -> a -> IO ()
debugTest name value = do
    let str : VString = encode value
    let result = show str
    putStrLn $ "\n\nRunning test:\n=============\n" ++ name
    putStrLn $ "Value:\n=============\n\n" ++ result
    putStrLn "\n\nTest completed.\n\n"

public export 
removeSpaces : String -> String
removeSpaces = pack . (filter (not . isSpace)) . unpack
public export
debugFileTest : Encode a VString => String -> a -> IO ()
debugFileTest file value = do
    let str : VString = encode value
    let result = show str
    _ <- writeFile ("generated/" ++ (removeSpaces file) ++ ".ll") result
    putStrLn $ "Test output written to " ++ file
public export
encodeTest : Encode a VString => String -> a -> String -> IO ()
encodeTest name value expected = do
    let str : VString = encode value
    let result = show str
    if result == expected 
      then
          putStrLn $ "Test " ++ name ++ " passed."
      else
          putStrLn $ "Test " ++ name ++ " failed. Expected: " ++ expected ++ ", got: " ++ result