module Main

import Test.Helper
import Test.Groups
import Test.Groups.Encoding

main : IO ()
main = do 
    putStrLn "Starting LLVM Library Tests..."
    putStrLn "================================"
    
    -- Run encoding tests to show LLVM op representations
    putStrLn "\nRunning LLVM Encoding Tests:"
    runTestSuite encodingTestsSuite
    
    -- Run all other test suites
    let allTestSuites = Test.Groups.allSuites
    runAllSuites allTestSuites
    
    putStrLn "\nTest run complete."