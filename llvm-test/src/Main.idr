module Main

import Test.Helper
import Test.Groups
import Test.Groups.Encoding

main : IO ()
main = do 
    putStrLn "Running tests..."
    allTests