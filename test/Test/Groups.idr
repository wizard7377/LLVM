module Test.Groups

import Data.LLVM
import Data.LLVM.Core
import Data.LLVM.Class
import Test.Groups.Module
import Test.Helper
import Test.Groups.Encoding
public export
allTests : IO ()
allTests = do
    putStrLn "Starting all tests..."
    encodingTests
    moduleTests
    putStrLn "All tests completed."