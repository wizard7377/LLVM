module Test.Helper

import Data.List
import Data.String
import Data.LLVM.Class
public export
debugTest : Encode a String => String -> a -> IO ()
debugTest name value = do
    putStrLn $ "Running test: " ++ name
    putStrLn $ "Value: " ++ encode value
    putStrLn "Test completed."
public export
encodeTest : Encode a String => String -> a -> String -> IO ()
encodeTest name value expected = do
    let result = encode value
    if result == expected 
      then
          putStrLn $ "Test " ++ name ++ " passed."
      else
          putStrLn $ "Test " ++ name ++ " failed. Expected: " ++ expected ++ ", got: " ++ result